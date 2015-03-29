{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE NoRebindableSyntax #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Library for interacting with the SuperCollider server.
-- 
--   You don't need to use much of this day-to-day
-- 
--   There's a toplevel 'scServerState' that stores the current state of the SC server
module Vivid.SCServer (
     call
   , callBS
   , quit
   , cmdPeriod

   , NodeId(..)
   , newNodeId

   , BufferId(..)
   , newBufferId
   , setMaxBufferIds
   , makeBuffer
   , makeBufferFromFile
   , saveBuffer

   , createSCServerConnection
   , callAndWaitForDone

   , SCServerState(..)
   , scServerState
   ) where

import Vivid.OSC
import Vivid.SynthDef.Types

import Network.Socket (SocketType(Datagram), defaultProtocol, socket, AddrInfo(..), getAddrInfo, Socket, HostName, ServiceName, connect)
import Network.Socket.ByteString

import Control.Concurrent (threadDelay)
--import qualified Data.ByteString as B hiding (find, elem)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Control.Concurrent.STM as STM

{-
import qualified Data.Map as Map
import Data.Map (Map)
-}
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.ByteString.Char8 as BS8

-- We use this only for "the unsafePerformIO hack"
-- (https://wiki.haskell.org/Top_level_mutable_state) so that functions can
-- refer to the state without being passed the state explicitly. This should
-- still be safe:
import System.IO.Unsafe (unsafePerformIO)

-- SETTINGS:
defaultSCServerPort :: String
defaultSCServerPort = "57110"
-- defaultSCLangPort   = "57120"


{-# NOINLINE scServerState #-}
scServerState :: SCServerState
-- see the above note about this use of unsafePerformIO:
scServerState = unsafePerformIO makeEmptySCServerState

newtype NodeId
      = NodeId { unNodeId :: Int32 }
   deriving (Show, Eq)

newtype BufferId
      = BufferId { unBufferId :: Int32 }
   deriving (Show, Eq)

data SCServerState
   = SCServerState
     { scServer_socket :: !(TVar (Maybe Socket))
     , scServer_availableBufferIds :: !(TVar [BufferId])
     , scServer_maxBufIds :: !(TVar Int32)
     , scServer_availableNodeIds :: !(TVar [NodeId])
     , scServer_availableSyncIds :: !(TVar [SyncId])
     , scServer_definedSDs :: !(TVar (Set (SDName, Int))) -- Int is the hash
     }

-- | Stop the SuperCollider server
quit :: IO ()
quit = call $ OSC "/quit" []

-- | __You usually don't need to call this function__
-- 
--   Use this if to connect on a non-default port or to a server not at localhost
-- 
--   Otherwise the connection is created when it's needed.
--   You can also use this to explicitly create the connection, so the
--   computation is done upfront
-- 
--   The 'HostName' is the ip address or "localhost". The 'ServiceName' is the port
createSCServerConnection :: HostName -> ServiceName -> IO Socket
createSCServerConnection hostName port = do
   let !_ = scServerState
   readTVarIO (scServer_socket scServerState) >>= \case
      Nothing -> do
         s <- connectToSCServer hostName port
         (atomically . (writeTVar $ scServer_socket scServerState) . Just) s
         return s
      Just _ -> error "Too late -- connection already established. Disconnect first."

connectToSCServer :: HostName -> ServiceName -> IO Socket
connectToSCServer hostName port = do
   (serverAddr:_) <- getAddrInfo Nothing (Just hostName) (Just port)
   s <- socket (addrFamily serverAddr) Datagram defaultProtocol
   connect s (addrAddress serverAddr)
   _ <- send s $ encodeOSC $ OSC "/dumpOSC" [OSC_I 1]
   _ <- send s $ encodeOSC $ OSC "/g_new" [OSC_I 1, OSC_I 0, OSC_I 0]
   threadDelay $ fromEnum 1e3
   return s

getSCServerSocket :: IO Socket
getSCServerSocket = getSCServerSocket' scServerState

getSCServerSocket' :: SCServerState -> IO Socket
getSCServerSocket' scServerState' = do
   let !_ = scServerState'
   readTVarIO (scServer_socket scServerState') >>= \case
      Nothing -> do
         s <- connectToSCServer "localhost" defaultSCServerPort
         (atomically . (writeTVar $ scServer_socket scServerState') . Just) s
         return s
      Just s -> return s

makeEmptySCServerState :: IO SCServerState
makeEmptySCServerState = do
   sockTVar <- newTVarIO Nothing
   availBufIds <- newTVarIO $ drop 512 $ map BufferId $ cycle [0..]
   availNodeIds <- newTVarIO $ map NodeId [10000..] -- sclang starts at 2000
   maxBufIds <- newTVarIO 1024
   syncIds <- newTVarIO $ drop 10000 $ map SyncId $ cycle [0..]
   definedSDs <- newTVarIO $ Set.empty

   return $ SCServerState
          { scServer_socket = sockTVar
          , scServer_availableBufferIds = availBufIds
          , scServer_maxBufIds = maxBufIds
          , scServer_availableNodeIds = availNodeIds
          , scServer_availableSyncIds = syncIds
          , scServer_definedSDs = definedSDs
          }

-- | Send an 'OSC' message to the SuperCollider server
call :: OSC -> IO ()
call message = do
   let !_ = scServerState
   callBS (encodeOSC message)

-- | Async messages to the sc server get responded to with \"\/done\" -- so this calls those functions and waits for the \"\/done\" before continuing
callAndWaitForDone :: OSC -> IO ()
callAndWaitForDone message@(OSC _cmd _) = do
   s <- getSCServerSocket
   call message
   threadDelay $ fromEnum 1e4
   sid@(SyncId syncId) <- newSyncId
   call $ OSC "/sync" [OSC_I syncId]
   getDoneMessage s sid
 where
   getDoneMessage :: Socket -> SyncId -> IO ()
   getDoneMessage s sid@(SyncId syncId) = recvFrom s 1024 >>= \(msg, _) ->
      case decodeOSC msg of
         -- OSC "/done" [OSC_S cmdFinished] | cmd == cmdFinished -> return ()
         OSC "/synced" [OSC_I syncFinished] | syncFinished == syncId -> return ()
         _ -> getDoneMessage s sid

newtype SyncId
      = SyncId Int32
   deriving (Show, Read, Eq, Ord)

-- | Send a ByteString to the SuperCollider server.
--   You usually want to use 'call' instead. May be removed in future versions.
callBS :: ByteString -> IO ()
callBS message = do
   let !_ = scServerState

   sock <- getSCServerSocket

   _ <- send sock message
   return ()

{-
call' :: SCServerState -> OSC -> IO ()
call' scServerState' message = do
   let !_ = scServerState'

   sock <- getSCServerSocket' scServerState'

   _ <- send sock (encodeOSC message)
   return ()
-}

-- | Your \"emergency\" button. Run this and everything playing on the SC server
--   will be freed -- silence!
-- 
--   Corresponds to the cmd-. \/ ctrl-.  key command in the SuperCollider IDE
cmdPeriod :: IO ()
cmdPeriod = do
   call $ OSC "/g_freeAll" [OSC_I 0]
   call $ OSC "/clearSched" []
   call $ OSC "/g_new" [OSC_I 1, OSC_I 0, OSC_I 0]

newBufferId :: IO BufferId
newBufferId = do
   maxBufIds <- readTVarIO (scServer_maxBufIds scServerState)
   BufferId nn <- getNextAvailable scServer_availableBufferIds
   return . BufferId $ nn `mod` maxBufIds

getNextAvailable :: (SCServerState -> TVar [a]) -> IO a
getNextAvailable getter = do
   let !_ = scServerState
   atomically $ do
      let avail = getter scServerState
      (n:rest) <- readTVar avail
      writeTVar avail rest
      return n

newNodeId :: IO NodeId
newNodeId =
   getNextAvailable scServer_availableNodeIds

newSyncId :: IO SyncId
newSyncId =
   getNextAvailable scServer_availableSyncIds

-- | If you've started the SC server with a non-default number of buffer ids,
--   (e.g. with the \"-b\" argument), you can reflect that here
-- 
--   Note that the buffer ids start at 512, to not clash with any that
--   sclang has allocated
setMaxBufferIds :: Int32 -> IO ()
setMaxBufferIds newMax = atomically $
   writeTVar (scServer_maxBufIds scServerState) newMax

-- | Make an empty buffer
-- 
--   The Int32 is the buffer length /in samples/. Multiply seconds by
--   the default sample rate of the server (usually 48000) to get the number
--   of samples
makeBuffer :: Int32 -> IO BufferId
makeBuffer bufferLength = do
   bufId@(BufferId bufIdInt) <- newBufferId
   call $ OSC "/b_alloc" [
       OSC_I bufIdInt
      ,OSC_I bufferLength
      ,OSC_I 1
      ,OSC_I 0
      ]
   return bufId

-- | Make a buffer and fill it with sound data from a file
makeBufferFromFile :: FilePath -> IO BufferId
makeBufferFromFile fPath = do
   bufId@(BufferId bufIdInt) <- newBufferId
   call $ OSC  "/b_allocRead" [
        OSC_I bufIdInt
      , OSC_S (BS8.pack fPath)
      , OSC_I 0
      , OSC_I (-1)
      ]
   return bufId

-- | Write a buffer to a file
saveBuffer :: BufferId -> FilePath -> IO ()
saveBuffer (BufferId theBufId) fPath =
      call $ OSC "/b_write" [
         OSC_I theBufId
        ,OSC_S (BS8.pack fPath)
        ,OSC_S "wav"
        ,OSC_S "float"
        ]
