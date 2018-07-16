-- | This is for timing of actions that's more precise than IO
-- 
--   It tells the server when to perform the actions, so musical timing won't
--   be affected by e.g. network latency or the time it took to compute a value
-- 
--   If you're running vivid on a different computer than the SC synth, make
--   sure the clocks agree

-- {-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.Actions.Scheduled (
     Scheduled
   , doScheduledIn
   , doScheduledAt
   , doScheduledNow
   ) where

import Vivid.Actions.Class
import Vivid.Actions.IO () -- Just until we remove MonadIO
import Vivid.OSC
import Vivid.SCServer
import Vivid.SynthDef (SynthDef)

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (evalStateT, put, get, modify, StateT)
import Data.ByteString (ByteString)
import Prelude

type Scheduled = StateT Timestamp IO


instance VividAction Scheduled where
   callOSC :: OSC -> Scheduled ()
   callOSC message = do
      now <- getTime
      liftIO . callBS . encodeOSCBundle $ OSCBundle now [Right message]

   callBS :: ByteString -> Scheduled ()
   callBS message = do
      now <- getTime
      liftIO . callBS . encodeOSCBundle $ OSCBundle now [Left message]

   sync :: Scheduled ()
   sync = return () -- always right?

   waitForSync :: SyncId -> Scheduled ()
   waitForSync _ = return () -- always right?

   wait :: Real n => n -> Scheduled ()
   wait t = modify (`addSecs` realToFrac t)

   getTime :: Scheduled Timestamp
   getTime = get

   newBufferId :: Scheduled BufferId
   newBufferId = liftIO newBufferId

   newNodeId :: Scheduled NodeId
   newNodeId = liftIO newNodeId

   newSyncId :: Scheduled SyncId
   newSyncId = liftIO newSyncId

   fork :: Scheduled () -> Scheduled ()
   fork action = do
      timeOfFork <- get
      action
      put timeOfFork

   defineSD :: SynthDef a -> Scheduled ()
   defineSD = liftIO . void . forkIO . defineSD

-- | Schedule an action to happen at the given time
doScheduledAt :: Timestamp -> Scheduled a -> IO a
doScheduledAt startTime action =
   evalStateT action startTime

-- | Schedule an action to happen n seconds from now
doScheduledIn :: Double -> Scheduled a -> IO a
doScheduledIn numSecs action = do
   now <- getTime
   doScheduledAt (addSecs now numSecs) action

-- | Schedule an action to happen right now. Because of server latency this
--   could arrive late, so you might want to do something like
--   @doScheduledIn 0.01@ instead:
doScheduledNow :: Scheduled a -> IO a
doScheduledNow action = do
   now <- getTime
   doScheduledAt now action
