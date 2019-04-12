{-# LANGUAGE
     BangPatterns
   , LambdaCase
   , OverloadedStrings

   , NoIncoherentInstances
   , NoMonomorphismRestriction
   , NoUndecidableInstances
   #-}

module Vivid.SCServer.Connection (
     createSCServerConnection
   , defaultConnectConfig
   , defaultMessageFunction
   , ignoreMessagesFunction
   , SCConnectConfig(..)
   , closeSCServerConnection
   , ConnProtocol(..)
   , getMailboxForSyncId
   , getSCServerSocket
   , waitForSync_io
   , waitForSync_io_noGC
   ) where

import Vivid.SC.Server.Commands as SCCmd

import Vivid.OSC
import Vivid.OSC.Bundles (initTreeCommand)
import Vivid.SCServer.State

import Network.Socket (
     SocketType(Datagram {- , Stream -}), defaultProtocol, socket
   , AddrInfo(..), getAddrInfo
   -- , AddrInfoFlag(..), defaultHints
   , Socket, HostName, ServiceName, connect, close -- , listen, bind
   -- , bindSocket, accept
                                                            
    -- We put this everywhere we do socket actions for Windows compatibility:
   , withSocketsDo
   )

import Network.Socket.ByteString (send, recv)

import Control.Concurrent (forkIO, ThreadId, killThread)
import Control.Concurrent.MVar
import Control.Concurrent.STM -- (readTVar, modifyTVar', atomically, writeTVar, {- readTVarIO, -} swapTVar)
import Control.Monad (forever)
import Data.Int (Int32)
-- import Data.IORef
import qualified Data.Map as Map
import Data.Monoid

-- | __You usually don't need to call this function__
-- 
--   Use this if to connect on a non-default port or to a server not at localhost
-- 
--   Otherwise the connection is created when it's needed.
--   You can also use this to explicitly create the connection, so the
--   computation is done upfront
-- 
--   The 'HostName' is the ip address or "localhost". The 'ServiceName' is the port
createSCServerConnection :: SCConnectConfig -> IO (Either String Socket)
createSCServerConnection connConfig = do
   let !_ = scServerState
   shouldMakeSock scServerState >>= \case
      True -> makeSock scServerState connConfig >>= \case
         Just s -> pure $ Right s
         Nothing -> pure $ Left "Unable to create socket"
      False ->
         pure $ Left "Too late -- connection already established. Disconnect first."

-- | Explicitly close Vivid's connection to a SC server.
-- 
--   Day-to-day, you can usually just let your program run without using this.
-- 
--   For example though, if you're running code that uses Vivid in ghci, and
--   you ":r", you'll want to disconnect first -- there are processes running
--   which can step on the toes of your new instance
--   (TODO: this isn't fully true - I ":r" all the time - what do I mean here?)
-- 
--   Also if you want to change the params of your connection (e.g. to connect
--   to a different server), you'll want to disconnect from the other
--   connection first
closeSCServerConnection :: IO ()
closeSCServerConnection = do
   let !_ = scServerState
   ish <- atomically $ do
      writeTVar (_scServerState_socketConnectStarted scServerState) False
{-
      (,) <$> swapTVar (_scServerState_socket scServerState) Nothing
          <*> swapTVar (_scServerState_listener scServerState) Nothing
-}
      (,) <$> tryTakeTMVar (_scServerState_socket scServerState)
          <*> tryTakeTMVar (_scServerState_listener scServerState)

{-
   ish <- (,) <$> readIORef (_scServerState_socket scServerState)
              <*> readIORef (_scServerState_listener scServerState)
   writeIORef (_scServerState_socket scServerState) Nothing
   writeIORef (_scServerState_listener scServerState) Nothing
-}
   case ish of
      (Just sock, Just listener) -> do
         killThread listener
         withSocketsDo $ close sock
      (Nothing, Nothing) -> pure ()
      _ -> error "well that's weird"



data ConnProtocol
   = ConnProtocol_UDP
   -- ConnProtocol_TCP
 deriving (Show, Read, Eq, Ord)

data SCConnectConfig
   = SCConnectConfig {
    _scConnectConfig_hostName :: HostName
  , _scConnectConfig_port :: ServiceName
  , _scConnectConfig_clientId :: Int32
     -- ^ To prevent NodeId clashes when multiple clients are connected to
     --   the same server, each client should have a separate clientId, which
     --   keeps the nodeId separate. Sclang's default clientId is 0, and ours
     --   is 1, so you can run both at the same time without config.
  , _scConnectConfig_connProtocol :: ConnProtocol
  , _scConnectConfig_serverMessageFunction :: OSC -> IO ()
  -- max # of synthdefs -- and clear em out
  }
-- deriving (Show, Read, Eq)


-- | The default _scConnectConfig_clientId is 1, and sclang's is 0, so you should
--   be able to run vivid side-by-side with the SC IDE out of the box.
defaultConnectConfig :: SCConnectConfig
defaultConnectConfig = SCConnectConfig {
     _scConnectConfig_hostName = "127.0.0.1"
   , _scConnectConfig_port = "57110"
   , _scConnectConfig_clientId = 1
   , _scConnectConfig_connProtocol = ConnProtocol_UDP
   , _scConnectConfig_serverMessageFunction = defaultMessageFunction
   }

-- Internal -- this is what gets called after we check a socket doesn't
-- already exist:
connectToSCServer :: SCConnectConfig -> IO (Socket, ThreadId)
connectToSCServer scConnectConfig = withSocketsDo $ do
   let !_ = scServerState
   let hostName = _scConnectConfig_hostName scConnectConfig
       port = _scConnectConfig_port scConnectConfig
       connType = case _scConnectConfig_connProtocol scConnectConfig of
          ConnProtocol_UDP -> Datagram
--          ConnProtocol_TCP -> Stream
   (serverAddr:_) <- getAddrInfo Nothing {- (Just (defaultHints {addrFlags = [AI_PASSIVE]})) -} (Just hostName) (Just port)
   s <- socket (addrFamily serverAddr) connType defaultProtocol
   {-
   if (connType == Stream)
      then do
         print 0
         bindSocket s (addrAddress serverAddr)
         print 1
         listen s 1
         -- _ <- accept s
         pure ()
   else connect s (addrAddress serverAddr)
-}
   setClientId (_scConnectConfig_clientId scConnectConfig)
   connect s (addrAddress serverAddr)
--   accept s

   atomically $ writeTVar (_scServerState_serverMessageFunction scServerState) $
      _scConnectConfig_serverMessageFunction scConnectConfig
   listener <- forkIO $ startMailbox s
   let firstSyncID = toEnum $ numberOfSyncIdsToDrop - 2
   _ <- send s $ encodeOSCBundle $ OSCBundle (Timestamp 0) [
        Right $ SCCmd.dumpOSC DumpOSC_Parsed
      , Right $ initTreeCommand
      , Right $ SCCmd.sync (SyncId firstSyncID)
      ]
   waitForSync_io (SyncId firstSyncID)
   pure (s, listener)

waitForSync_io :: SyncId -> IO ()
waitForSync_io syncId = do
   _ <- readMVar =<< getMailboxForSyncId syncId
   -- We garbage-collect these so the Map stays small -- but it means you can only wait
   -- for a sync from one place:
   atomically $ modifyTVar' (_scServerState_syncIdMailboxes scServerState) $
      Map.delete syncId

waitForSync_io_noGC :: SyncId -> IO ()
waitForSync_io_noGC syncId = do
   _ <- readMVar =<< getMailboxForSyncId syncId
   pure ()

-- TODO: what's "mailbox" here? Is it like an Erlang mailbox, to receive and
-- dispatch all messages?
startMailbox :: Socket -> IO ()
startMailbox s = do
   let !_ = scServerState
   forever $ recv {- From -} s 65536 >>= \(msg{- , _ -}) ->
      case decodeOSC msg of
         Right (OSC "/synced" [OSC_I theSyncId]) -> do
            syncBox <- getMailboxForSyncId (SyncId theSyncId)
            tryPutMVar syncBox () >>= \case
               True -> pure ()
               False ->
                  putStrLn $
                     "That's weird!: we got the same syncId twice: "
                     ++ show theSyncId
         Right x -> do
            otherMessageFunction <- readTVarIO $
               _scServerState_serverMessageFunction scServerState
            otherMessageFunction x
         Left e -> putStrLn $ "ERROR DECODING OSC: " ++ show (msg, e)

-- | Print all messages other than \"/done\"s
defaultMessageFunction :: OSC -> IO ()
defaultMessageFunction = \case
   -- Some examples you might want to handle individually:
   {-
   OSC "/fail" [OSC_S "/blah", OSC_S "Command not found"] -> pure ()
   OSC "/fail" [OSC_S "/s_new", OSC_S "wrong argument type"] -> pure ()
   OSC "/fail" [OSC_S "/b_allocRead", OSC_S "File 'blah.ogg' could not be opened: Error : flac decoder lost sync.\n",OSC_I 2]
   -}
   OSC "/done" [OSC_S _] -> pure ()
   OSC "/done" [OSC_S _, OSC_I _] -> pure ()
   x -> putStrLn $ "Msg from server: " <> show x

-- | If you don't want to hear what the server has to say
ignoreMessagesFunction :: OSC -> IO ()
ignoreMessagesFunction _ = pure ()

-- This is a nice example of when STM can be really helpful -
-- It's impossible! (right?) to have 2 threads create mailboxes and have em overwrite each
-- other -- so we can make a guarantee about recieving a sync that you register for
getMailboxForSyncId :: SyncId -> IO (MVar ())
getMailboxForSyncId syncId = do
   mvarThatIMightWannaUse <- newEmptyMVar
   atomically $ do
      allMailboxes <- readTVar (_scServerState_syncIdMailboxes scServerState)
      case Map.lookup syncId allMailboxes of
         Just syncBox -> pure syncBox
         Nothing -> do
            writeTVar (_scServerState_syncIdMailboxes scServerState)
              (Map.insert syncId mvarThatIMightWannaUse allMailboxes)
            pure mvarThatIMightWannaUse

getSCServerSocket :: IO Socket
getSCServerSocket = getSCServerSocket' scServerState

getSCServerSocket' :: SCServerState -> IO Socket
getSCServerSocket' scServerState' = do
   let !_ = scServerState'
   shouldMakeSock scServerState' >>= \case
      True -> do
         makeSock scServerState' defaultConnectConfig >>= \case
            Just x -> pure x
            Nothing -> error "Unexpected failure creating socket"
      False -> atomically . readTMVar $ _scServerState_socket scServerState'

shouldMakeSock :: SCServerState -> IO Bool
shouldMakeSock serverState = atomically $ do
   let theVar = _scServerState_socketConnectStarted serverState
   alreadyBeingMade <- readTVar theVar
   case alreadyBeingMade of
      True -> pure False
      False -> do
         writeTVar theVar True
         pure True

makeSock :: SCServerState -> SCConnectConfig -> IO (Maybe Socket)
makeSock serverState connConfig = do
   (sock, listener) <- connectToSCServer connConfig
   atomically $ (do
      -- writeTVar (_scServerState_socket serverState) $ Just sock
      -- writeTVar (_scServerState_listener serverState) $ Just listener
      a <- tryPutTMVar (_scServerState_socket serverState) sock
      b <- tryPutTMVar (_scServerState_listener serverState) listener
      check $ a && b
      pure $ Just sock)
         `orElse` (pure Nothing)

