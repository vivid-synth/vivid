-- | This is unscheduled - the server will do what you tell it to
--   as soon as it can. That can mean there'll be slight delays
--   because of time it took to compute what to do or because of
--   network latency. If you want more precise timing look at
--   "Scheduled"
-- 
--   Doing \"VividAction\"s in IO can be like a sketchpad:
--   it's the quickest way to get an idea out.
--   The cool thing is you can take an action that you're sketching
--   and put a function in front of it to get more precise timing
--   E.g. if you have the function:
--
--   @
--   playTone = do
--      synth <- play $ 0.1 ~* sinOsc (freq_ 440)
--      wait 1
--      free synth
--   @
-- 
--   You can play it quickly with just:
-- 
--   > playTone
-- 
--   But if you want precise timing all you need to do is say e.g.:
-- 
--   > playScheduledIn 0.01 playTone

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.Actions.IO (
     defineSDFromFile
   ) where

import Vivid.Actions.Class
import Vivid.OSC (OSC(..), OSCDatum(..), encodeOSC, Timestamp(..), timestampFromUTC)
-- import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SC.Server.Commands as SCCmd
import Vivid.SCServer.State (BufferId(..), NodeId(..), SyncId(..), getNextAvailable, scServerState, SCServerState(..))
import Vivid.SCServer.Connection ({-getMailboxForSyncId,-} getSCServerSocket, waitForSync_io)
import Vivid.SynthDef

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (readTVarIO, atomically, modifyTVar)
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (writeFile)
import Data.Hashable
import qualified Data.Set as Set
import Data.Time (getCurrentTime)
import System.Directory (getTemporaryDirectory)

import Network.Socket (withSocketsDo) -- We add this everywhere for Windows compat
import Network.Socket.ByteString (send)

instance VividAction IO where
   callOSC :: OSC -> IO ()
   callOSC message = callBS (encodeOSC message)

   callBS :: ByteString -> IO ()
   callBS message = do
      let !_ = scServerState
      sock <- getSCServerSocket
      -- TODO: if TCP, prefix with the length ("## int - the length in bytes of the following message.") from Server-Architecture.schelp
      _ <- withSocketsDo $ send sock message
      return ()

   sync :: IO ()
   sync = do
      wait (0.01 :: Float) -- Just to make sure you don't "sync" before calling
                           --   the command you want to sync (temporary)
      sid <- newSyncId
      callOSC $ SCCmd.sync sid
      waitForSync sid

   waitForSync :: SyncId -> IO ()
   waitForSync = waitForSync_io

   wait :: Real n => n -> IO ()
   wait t = threadDelay $ round (realToFrac (t * 10^(6::Int)) :: Double)

   getTime :: IO Timestamp
   getTime = timestampFromUTC <$> getCurrentTime

   newBufferId :: IO BufferId
   newBufferId = do
      maxBufIds <- readTVarIO (_scServerState_maxBufIds scServerState)
      BufferId nn <- getNextAvailable _scServerState_availableBufferIds
      return . BufferId $ nn `mod` maxBufIds

   newNodeId :: IO NodeId
   newNodeId = getNextAvailable _scServerState_availableNodeIds

   newSyncId :: IO SyncId
   newSyncId =
      getNextAvailable _scServerState_availableSyncIds

   fork :: IO () -> IO ()
   fork action = do
      _ <- forkIO action
      return ()

   defineSD :: SynthDef a -> IO ()
   defineSD synthDef@(SynthDef name _ _) = do
      let !_ = scServerState
      hasBeenDefined <- (((name, hash synthDef) `Set.member`) <$>) $
         readTVarIO (_scServerState_definedSDs scServerState)
      unless hasBeenDefined $ do
         oscWSync $ \syncId ->
            callOSC $
               SCCmd.d_recv [sdToLiteral synthDef] (Just $ SCCmd.sync syncId)
         atomically $ modifyTVar (_scServerState_definedSDs scServerState) $
            ((name, hash synthDef) `Set.insert`)

-- | Synchronous
defineSDFromFile :: SynthDef a -> IO ()
defineSDFromFile theSD = do
   tempDir <- getTemporaryDirectory
   let fName = tempDir++"/" ++ show (hash theSD) ++ ".scsyndef"
   BS.writeFile fName $ encodeSD theSD
   oscWSync $ \syncId ->
      callOSC $ SCCmd.d_load fName (Just $ SCCmd.sync syncId)

