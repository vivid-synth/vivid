-- | Non-realtime synthesis. Create a sound file from the same instructions
--   you use to perform live!
-- 
--   **Note** we don't currently support redefining Synthdefs midway -- e.g.
--   you can't explicitly define a SynthDef "foo" (with 'defineSD'), then make a
--   synth from it, then explicitly define it again with a new definition, and
--   then make a new synth with the new definition


{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.Actions.NRT (
     NRT -- (..) -- ^ May not be exported in the future
   , writeNRT
   , writeNRTScore
   , runNRT

   , writeNRTWith
   , NRTArgs(..)
   , defaultNRTArgs
   ) where

import qualified Vivid.SC.Server.Commands as SCCmd

import Vivid.Actions.Class
import Vivid.Actions.IO () -- maybe not in the future
import Vivid.OSC
import Vivid.OSC.Bundles (encodeOSCBundles)
import Vivid.SCServer
-- import Vivid.SCServer.State
import Vivid.SynthDef (encodeSD, sdToLiteral)
import Vivid.SynthDef.Types

import Control.Applicative
-- import Control.Arrow (first, second)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, modify, execStateT, StateT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (writeFile)
import qualified Data.ByteString.UTF8 as UTF8
import Data.Char (toLower)
import Data.Hashable (hash)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import qualified Data.Set as Set
import System.Exit
import System.FilePath (takeExtension)
import System.Process (system)
import Prelude

-- We keep track of the maximum timestamp so that the generated audio file doesn't cut off before a final 'wait' finishes:
type NRT = StateT (Timestamp, Maximum Timestamp, Map Timestamp [Either ByteString OSC]) IO

instance VividAction NRT where
   callOSC :: OSC -> NRT ()
   callOSC message = do
      now <- getTime
      modify ((\f (a,b,c)->(a,b,f c)) (Map.insertWith (<>) now [Right message]))

   callBS :: ByteString -> NRT ()
   callBS message = do
      now <- getTime
      modify $ \(a,b,x) ->
         (a, b, Map.insertWith (<>) now [Left message] x)

   sync :: NRT ()
   sync = return ()

   waitForSync :: SyncId -> NRT ()
   waitForSync _ = return ()

   wait :: Real n => n -> NRT ()
   wait t = modify $ \(oldT, maxT, c) ->
      let newT = oldT `addSecs` realToFrac t
      in (newT, Maximum newT `max` maxT, c)

   getTime :: NRT Timestamp
   getTime = (\(t,_,_) -> t) <$> get

   newBufferId :: NRT BufferId
   newBufferId = liftIO newBufferId

   newNodeId :: NRT NodeId
   newNodeId = liftIO newNodeId

   newSyncId :: NRT SyncId
   newSyncId = liftIO newSyncId

   fork :: NRT () -> NRT ()
   fork action = do
      (timeOfFork, oldMaxTime, _) <- get
      action
      modify $ \(_timeAfterFork_ignore, newMaxTime, c) ->
           -- this 'max' probably isn't necessary:
         (timeOfFork, newMaxTime `max` oldMaxTime :: Maximum Timestamp, c)

   defineSD :: SynthDef a -> NRT ()
   defineSD synthDef = do
      modify . (\f (a,b,c)->(a,b,f c)) $ Map.insertWith mappendIfNeeded (Timestamp 0) [
           Right $ SCCmd.d_recv [sdToLiteral synthDef] Nothing
         ]
    where
      mappendIfNeeded :: (Ord a) {- , Monoid m)-} => [a] -> [a] -> [a]
      mappendIfNeeded maybeSubset maybeSuperset =
         if Set.fromList maybeSubset `Set.isSubsetOf` Set.fromList maybeSuperset
            then maybeSuperset
            else maybeSubset <> maybeSuperset

runNRT :: NRT a -> IO [OSCBundle]
runNRT action = do
   (_, Maximum maxTSeen, protoBundles_woLast)
      <- execStateT action (Timestamp 0, Maximum (Timestamp 0), Map.empty)
   let protoBundles = Map.insertWith (<>) maxTSeen [] protoBundles_woLast
   return [ OSCBundle t as | (t, as) <- Map.toAscList protoBundles ]


-- | Generate a file of actions that SC can use to do NRT with.
-- 
--   __If you just want the .aiff file, you probably want 'writeNRT' instead.__
-- 
--   Usage: this will create a file at "/tmp/NRTout.aiff" with your @sound :: NRT a@:
-- 
--   > writeNRT "/tmp/foo.osc" test
--   > scsynth -N /tmp/foo.osc _ /tmp/NRTout.aiff 44100 AIFF int16
writeNRTScore :: FilePath -> NRT a -> IO ()
writeNRTScore path action =
   (BS.writeFile path . encodeOSCBundles) =<< runNRT action


-- | Generate an audio file from an NRT action -- this can write songs far faster
--   than it would take to play them.
-- 
--   This uses 'defaultNRTArgs' for its sample rate, number of channels, etc.
--   If you want to use args other than the default, use 'writeNRTWith'.
-- 
--   The file type is detected from its extension.
--   The extensions supported at the moment are .aif, .aiff, and .wav
-- 
--   (Mac OS X users will need to make sure 'scsynth' is in their $PATH)
-- 
--   (And I apologize, but I really don't know what Windows users will need to do)
-- 
--   Currently doesn't work with single quotes in the filename
writeNRT :: FilePath -> NRT a -> IO ()
writeNRT = writeNRTWith defaultNRTArgs

writeNRTWith ::  NRTArgs -> FilePath -> NRT a -> IO ()
writeNRTWith nrtArgs fPath nrtActions = do
   when ('\'' `elem` fPath) $ error "Didnt have time to implement filepaths with single quotes"
   contents <- encodeOSCBundles <$> runNRT nrtActions

   --  ${SHELL}
   system "/bin/sh -c 'which scsynth > /dev/null'" >>= \case
      ExitSuccess -> return ()
      ExitFailure _ -> error "No 'scsynth' found! Be sure to put it in your $PATH"
   let tempFile = "/tmp/vivid_nrt_" <> (show . hash) contents <> ".osc"
       !fileType =
          case Map.lookup (map toLower $ takeExtension fPath) extensionMap of
             Just x -> x
             Nothing -> error $
                "The only file extensions we currently understand are: "
                ++ show (Map.keys extensionMap)
       extensionMap = Map.fromList [
            (".aif", "AIFF")
          , (".aiff", "AIFF")
          , (".wav", "WAV")
            -- todo: these formats seem not to work:
          -- ".flac" -> "FLAC"
          -- ".ogg" -> "vorbis"
          ]

   BS.writeFile tempFile contents
   ExitSuccess <- system $ mconcat [
        --  ${SHELL}
        "/bin/sh -c "
      , " \"" -- Note these beginning and ending quotes
      , " scsynth"
      , " -o ", show $ _nrtArgs_numChans nrtArgs
      , " -N "
      , tempFile
      , " _ '", fPath, "' "
      , show $ _nrtArgs_sampleRate nrtArgs," ", fileType, " int16 "
      , " \""
      ]
   return ()


data NRTArgs
   = NRTArgs {
    _nrtArgs_sampleRate :: Int
   ,_nrtArgs_numChans :: Int
   }
 deriving (Show, Read, Eq, Ord)

defaultNRTArgs :: NRTArgs
defaultNRTArgs = NRTArgs {
    _nrtArgs_sampleRate = 48000
   ,_nrtArgs_numChans = 2
   }

-- Given an explicit type and tag so we don't accidentally  get the wrong element out of the tuple anywhere:
newtype Maximum a = Maximum a

instance (Eq a, Ord a) => Ord (Maximum a) where
   compare (Maximum a) (Maximum b) = compare a b
   Maximum a <= Maximum b = a <= b
   Maximum a < Maximum b = a < b
   Maximum a > Maximum b = a > b
   Maximum a >= Maximum b = a >= b
   max (Maximum a) (Maximum b) = Maximum $ max a b
   min (Maximum a) (Maximum b) = Maximum $ min a b

instance Eq a => Eq (Maximum a) where
   Maximum a == Maximum b = a == b
