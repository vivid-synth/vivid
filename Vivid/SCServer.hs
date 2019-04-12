{-# OPTIONS_HADDOCK show-extensions #-}

-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoRebindableSyntax #-}
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
   , Synth(..)
   , Group(..)
   , ParGroup(..)
   , defaultGroup

   -- * Buffers

   , BufferId(..)
   , makeBuffer
   , makeBufferFromFile
   , newBuffer
   , newBufferFromFile
   , newBufferFromFileBetween
   , saveBuffer
   , writeBuffer
   , writeBufferWith
   , WriteBufArgs(..)
   , defaultWBArgs
   , closeBuf
   , closeBuffer
   , zeroBuf

   -- * Manual management of SC server connection

   , createSCServerConnection
   , closeSCServerConnection
   , SCConnectConfig(..)
   , defaultConnectConfig

   , module Vivid.SCServer.State

   , shrinkSynthArgs

   ) where

import Vivid.OSC
import Vivid.OSC.Bundles (initTreeCommand)
import qualified Vivid.SC.Server.Commands as SCCmd
import Vivid.SC.Server.Types (Group(..), ParGroup(..))
import qualified Vivid.SC.Server.Commands as SCCmd

import Vivid.Actions.Class
import Vivid.SCServer.Connection
import Vivid.SCServer.State
import Vivid.SCServer.Types

import qualified Data.ByteString.UTF8 as UTF8 (fromString)
import Data.Int (Int32)
-- BBP hack:
import Prelude


-- | Your \"emergency\" button. Run this and everything playing on the SC server
--   will be freed -- silence!
-- 
--   Corresponds to the cmd-. \/ ctrl-.  key command in the SuperCollider IDE
cmdPeriod :: (VividAction m) => m ()
cmdPeriod = do
   callOSC $ SCCmd.g_freeAll [NodeId 1] -- 1 instead of 0 is temp! (is it? 1 is default group...)
   callOSC $ SCCmd.clearSched
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
newBuffer :: VividAction m => Int32 -> m BufferId
newBuffer bufferLength = do
   bufId <- newBufferId
   oscWSync $ \syncId ->
      callOSC $
         SCCmd.b_alloc bufId bufferLength 1 (Just $ SCCmd.sync syncId)
   return bufId

-- | Make a buffer and fill it with sound data from a file
-- 
--   The file path should be absolute (not relative), and if you're connecting
--   to a non-localhost server don't expect it to be able to read files from
--   your local hard drive!
-- 
--   Note that like 'makeBuffer' this is synchronous
newBufferFromFile :: (VividAction m) => FilePath -> m BufferId
newBufferFromFile = newBufferFromFileBetween 0 Nothing

newBufferFromFileBetween :: VividAction m => Int32 -> Maybe Int32 -> FilePath -> m BufferId
newBufferFromFileBetween startTime endTimeMay fPath = do
   bufId <- newBufferId
   oscWSync $ \syncId -> callOSC $
      SCCmd.b_allocRead bufId fPath startTime endTimeMay (Just $ SCCmd.sync syncId)
   return bufId

makeBufferFromFile :: (VividAction m) => FilePath -> m BufferId
makeBufferFromFile = newBufferFromFile

makeBuffer :: (VividAction m) => Int32 -> m BufferId
makeBuffer = newBuffer

-- | Write a buffer to a file
-- 
--   Alias of 'writeBuffer'
-- 
--   Synchronous.
saveBuffer :: (VividAction m) => BufferId -> FilePath -> m ()
saveBuffer = writeBuffer

writeBuffer :: VividAction m => BufferId -> FilePath -> m ()
writeBuffer = writeBufferWith defaultWBArgs

writeBufferWith :: VividAction m => WriteBufArgs -> BufferId -> FilePath -> m ()
writeBufferWith args bufId fPath =
   oscWSync $ \syncId -> callOSC $
      SCCmd.b_write
         bufId
         fPath
         "wav"
         "float"
         -- Num frames:
         Nothing
         -- Start frame:
         0
         -- Whether to leave the file open (useful for diskOut)
         (_wb_keepOpen args)
         -- We make this synchronous because what if you send a
         -- "/b_write" then a "/quit"?(!):
         (Just $ SCCmd.sync syncId)

-- | We may add arguments in the future ; to future-proof your code, just update
--   fields of 'defaultWBArgs'
data WriteBufArgs
   = WriteBufArgs {
    _wb_keepOpen :: Bool
   }
 deriving (Show, Read, Eq, Ord)

defaultWBArgs :: WriteBufArgs
defaultWBArgs = WriteBufArgs {
     _wb_keepOpen = False
   }

-- | Close an open soundfile and write header information
-- 
--   Synchronous
closeBuffer :: VividAction m => BufferId -> m ()
closeBuffer bufId = oscWSync $ \syncId ->
   callOSC $ SCCmd.b_close bufId (Just $ SCCmd.sync syncId)

closeBuf :: VividAction m => BufferId -> m ()
closeBuf = closeBuffer

-- | Zero the sample data in a buffer
-- 
--   Synchronous
zeroBuf :: VividAction m => BufferId -> m ()
zeroBuf bufId = oscWSync $ \syncId ->
   callOSC $ SCCmd.b_zero bufId (Just $ SCCmd.sync syncId)

-- More info is available in HelpSource/Reference/default_group.schelp
defaultGroup :: Group ; defaultGroup = Group (NodeId 1)
