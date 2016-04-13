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

import Vivid.Actions.Class
import Vivid.Actions.IO () -- maybe not in the future
import Vivid.OSC
import Vivid.SCServer
-- import Vivid.SCServer.State
import Vivid.SynthDef (encodeSD)
import Vivid.SynthDef.Types

import Control.Applicative
import Control.Arrow (first, second)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, modify, execStateT, StateT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (writeFile)
import qualified Data.ByteString.Char8 as BS8 (pack)
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

type NRT = StateT (Timestamp, Map Timestamp [Either ByteString OSC]) IO

instance VividAction NRT where
   callOSC :: OSC -> NRT ()
   callOSC message = do
      now <- getTime
      modify (second (Map.insertWith (<>) now [Right message]))

   callBS :: ByteString -> NRT ()
   callBS message = do
      now <- getTime
      modify (second (Map.insertWith (<>) now [Left message]))

   sync :: NRT ()
   sync = return ()

   waitForSync :: SyncId -> NRT ()
   waitForSync _ = return ()

   wait :: (RealFrac n) => n -> NRT ()
   wait t = modify (first (`addSecs` realToFrac t))

   getTime :: NRT Timestamp
   getTime = fst <$> get

   newBufferId :: NRT BufferId
   newBufferId = liftIO newBufferId

   newNodeId :: NRT NodeId
   newNodeId = liftIO newNodeId

   newSyncId :: NRT SyncId
   newSyncId = liftIO newSyncId

   fork :: NRT () -> NRT ()
   fork action = do
      (timeOfFork, _) <- get
      action
      modify (first (\_ -> timeOfFork))

   defineSD :: SynthDef a -> NRT ()
   defineSD synthDef = do
      modify . second $ Map.insertWith mappendIfNeeded (Timestamp 0) [
           Right $ OSC (BS8.pack "/d_recv") [
                OSC_B $ encodeSD synthDef
              , OSC_I 0
              ]
         ]
    where
      mappendIfNeeded :: (Ord a) {- , Monoid m)-} => [a] -> [a] -> [a]
      mappendIfNeeded maybeSubset maybeSuperset =
         if Set.fromList maybeSubset `Set.isSubsetOf` Set.fromList maybeSuperset
            then maybeSuperset
            else maybeSubset <> maybeSuperset

runNRT :: NRT a -> IO [OSCBundle]
runNRT action = do
   (_, protoBundles) <- execStateT action (Timestamp 0, Map.empty)
   return [ OSCBundle t as | (t, as) <- Map.toList protoBundles ]


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
       !fileType = case map toLower $ takeExtension fPath of
          ".aif" -> "AIFF"
          ".aiff" -> "AIFF"
          ".wav" -> "WAV"
          _ -> error "The only file extensions we currently understand are .wav, .aif, and .aiff"

   BS.writeFile tempFile contents
   ExitSuccess <- system $ mconcat [
        --  ${SHELL}
        "/bin/sh -c \"scsynth -N "
      , tempFile
      , " _ '", fPath, "' "
      , show $ _nrtArgs_sampleRate nrtArgs," ", fileType, " int16\""
      ]
   return ()


data NRTArgs
   = NRTArgs {
    _nrtArgs_sampleRate :: Int
   }
 deriving (Show, Read, Eq, Ord)

defaultNRTArgs :: NRTArgs
defaultNRTArgs = NRTArgs 44100
