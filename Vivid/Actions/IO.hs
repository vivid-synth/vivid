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

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.Actions.IO (
   ) where

import Vivid.Actions.Class
import Vivid.OSC (OSC(..), OSCDatum(..), encodeOSC, Timestamp(..), utcToTimestamp)
import Vivid.SCServer.State (BufferId(..), NodeId(..), SyncId(..), getNextAvailable, scServerState, SCServerState(..))
import Vivid.SCServer.Connection ({-getMailboxForSyncId,-} getSCServerSocket, waitForSync_io)
import Vivid.SynthDef

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (readTVarIO, atomically, modifyTVar)
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8 (pack)
import Data.Hashable
import qualified Data.Set as Set
import Data.Time (getCurrentTime)

import Network.Socket (withSocketsDo) -- We add this everywhere for Windows compat
import Network.Socket.ByteString (send)

instance VividAction IO where
   callOSC :: OSC -> IO ()
   callOSC message = callBS (encodeOSC message)

   callBS :: ByteString -> IO ()
   callBS message = do
      let !_ = scServerState
      sock <- getSCServerSocket
      _ <- withSocketsDo $ send sock message
      return ()

   sync :: IO ()
   sync = do
      wait (0.01 :: Float) -- Just to make sure you don't "sync" before calling
                           --   the command you want to sync (temporary)
      sid@(SyncId syncId) <- newSyncId
      callOSC $ OSC "/sync" [OSC_I syncId]
      waitForSync sid

   waitForSync :: SyncId -> IO ()
   waitForSync = waitForSync_io

   wait :: (RealFrac n) => n -> IO ()
   wait t = threadDelay $ round (t * 10^(6::Int))

   getTime :: IO Timestamp
   getTime = utcToTimestamp <$> getCurrentTime

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
         syncId@(SyncId syncIdInt) <- newSyncId
         callOSC $ OSC (BS8.pack "/d_recv") [
              OSC_B $ encodeSD synthDef
            , OSC_B . encodeOSC $ OSC "/sync" [OSC_I syncIdInt]
            ]
         waitForSync syncId
         atomically $ modifyTVar (_scServerState_definedSDs scServerState) $
            ((name, hash synthDef) `Set.insert`)
