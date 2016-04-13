{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE NoRebindableSyntax #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}


-- | Library for interacting with the SuperCollider server.
-- 
--   You don't need to use much of this day-to-day
-- 
--   There's a toplevel 'scServerState' that stores the current state of the SC server
module Vivid.SCServer (
     cmdPeriod
   , freeAll
   , Timestamp(..)
     
   -- * Nodes

   , NodeId(..)
   , Node(..)

   -- * Buffers

   , BufferId(..)
   , makeBuffer
   , makeBufferFromFile
   , newBuffer
   , newBufferFromFile
   , saveBuffer

   -- * Manual management of SC server connection

   , createSCServerConnection
   , closeSCServerConnection
   , SCConnectConfig(..)
   , defaultConnectConfig

   , module Vivid.SCServer.State

   , shrinkNodeArgs

   ) where

import Vivid.Actions.Class
import Vivid.OSC
import Vivid.SCServer.Connection
import Vivid.SCServer.State
import Vivid.SCServer.Types

import qualified Data.ByteString.Char8 as BS8 (pack)
import Data.Int (Int32)
-- BBP hack:
import Prelude


-- | Your \"emergency\" button. Run this and everything playing on the SC server
--   will be freed -- silence!
-- 
--   Corresponds to the cmd-. \/ ctrl-.  key command in the SuperCollider IDE
cmdPeriod :: (VividAction m) => m ()
cmdPeriod = do
   callOSC $ OSC "/g_freeAll" [OSC_I 0]
   callOSC $ OSC "/clearSched" []
   initTree
   
-- | Alias of 'cmdPeriod'
freeAll :: VividAction m => m ()
freeAll = cmdPeriod

initTree :: (VividAction m) => m ()
initTree = callOSC initTreeCommand

-- | Make an empty buffer
-- 
--   The Int32 is the buffer length /in samples/. Multiply seconds by
--   the default sample rate of the server (usually 48000) to get the number
--   of samples
-- 
--   Note that this is synchronous -- it doesn't return until the buffer is allocated
--   (in theory, this could hang if e.g. the UDP packet is lost)
newBuffer :: (VividAction m) => Int32 -> m BufferId
newBuffer bufferLength = do
   bufId@(BufferId bufIdInt) <- newBufferId
   syncId@(SyncId syncIdInt) <- newSyncId
   callOSC $ OSC "/b_alloc" [
       OSC_I bufIdInt
      ,OSC_I bufferLength
      ,OSC_I 1
      , OSC_B . encodeOSC $ OSC "/sync" [OSC_I syncIdInt]
      ]
   waitForSync syncId
   return bufId

-- | Make a buffer and fill it with sound data from a file
-- 
--   The file path should be absolute (not relative), and if you're connecting to
--   a non-localhost server don't expect it to be able to read files from your
--   hard drive!
-- 
--   Note that like "makeBuffer" this is synchronous
newBufferFromFile :: (VividAction m) => FilePath -> m BufferId
newBufferFromFile fPath = do
   bufId@(BufferId bufIdInt) <- newBufferId
   syncId@(SyncId syncIdInt) <- newSyncId
   callOSC $ OSC  "/b_allocRead" [
        OSC_I bufIdInt
      , OSC_S (BS8.pack fPath)
      , OSC_I 0
      , OSC_I (-1)
      , OSC_B . encodeOSC $ OSC "/sync" [OSC_I syncIdInt]
      ]
   waitForSync syncId
   return bufId

makeBufferFromFile :: (VividAction m) => FilePath -> m BufferId
makeBufferFromFile = newBufferFromFile

makeBuffer :: (VividAction m) => Int32 -> m BufferId
makeBuffer = newBuffer

-- | Write a buffer to a file
-- 
--   Synchronous.
saveBuffer :: (VividAction m) => BufferId -> FilePath -> m ()
saveBuffer (BufferId theBufId) fPath = do
   _syncId@(SyncId syncIdInt) <- newSyncId
   callOSC $ OSC "/b_write" [
      OSC_I theBufId
     ,OSC_S (BS8.pack fPath)
     ,OSC_S "wav"
     ,OSC_S "float"
     , OSC_I (-1)
     , OSC_I 0
     , OSC_I 0
       -- We make this synchronous because what if you send a "/b_write" then a "/quit"?(!):
     , OSC_B . encodeOSC $ OSC "/sync" [OSC_I syncIdInt]
     ]
