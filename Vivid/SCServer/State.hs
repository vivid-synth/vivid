{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.SCServer.State (
     BufferId(..)
   , NodeId(..)
   , SyncId(..)

   , scServerState
   , SCServerState(..)

   , setClientId
   , setMaxBufferIds

   , getNextAvailable
   , numberOfSyncIdsToDrop
   ) where

import Vivid.OSC (OSC)
import Vivid.SCServer.Types
import Vivid.SynthDef.Types

import Network.Socket (Socket)

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar
import Control.Concurrent.STM -- (readTVar, atomically, writeTVar, newTVar, TVar, TMVar)
import Control.Monad (when)
import Data.Bits
import Data.Int (Int32)
-- import Data.IORef
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Prelude

-- We use this only for "the unsafePerformIO hack"
-- (https://wiki.haskell.org/Top_level_mutable_state) so that functions can
-- refer to the state without being passed the state explicitly. This should
-- still be safe:
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE scServerState #-}
scServerState :: SCServerState
-- Currently you can only be connected to one SC server at a time. Future
--   versions plan to remove this.
-- See the above note about this use of unsafePerformIO:
scServerState = unsafePerformIO makeEmptySCServerState

data SCServerState
   = SCServerState
   -- We use 'IORef Maybe's instead of MVars so we can use weak pointer
   --   finalizers with older versions of GHC:
  { _scServerState_socketConnectStarted :: TVar Bool
  , _scServerState_socket :: !(TMVar Socket) -- !(TVar (Maybe Socket))
  , _scServerState_listener :: !(TMVar ThreadId) -- !(TVar (Maybe ThreadId))

  , _scServerState_availableBufferIds :: !(TVar [BufferId])
  , _scServerState_maxBufIds :: !(TVar Int32)
  , _scServerState_availableNodeIds :: !(TVar [NodeId])
  , _scServerState_availableSyncIds :: !(TVar [SyncId])
  , _scServerState_syncIdMailboxes :: !(TVar (Map SyncId (MVar ())))
  , _scServerState_serverMessageFunction :: !(TVar (OSC -> IO ()))
  , _scServerState_definedSDs :: !(TVar (Set (SDName, Int))) -- Int is the hash
  }

setClientId :: Int32 -> IO ()
setClientId clientId = do
   when (clientId < 0 || clientId > 31) $
      error "client id must be betw 0 and 31"
   atomically $ writeTVar (_scServerState_availableNodeIds scServerState) $
      -- The client id is the first 5 bits of a positive int:
      -- Note the incrementing gets weird once we hit the (.&.) -- should
      -- fix if anyone plans to use more than 33 million nodes
      (flip map) [1000..] $ \nodeNum -> NodeId $
         ((clientId `shiftL` ((finiteBitSize nodeNum-5)-1)) .|.) $
            ((maxBound `shiftR` 5) .&. nodeNum)

numberOfSyncIdsToDrop :: Int
numberOfSyncIdsToDrop = 10000

makeEmptySCServerState :: IO SCServerState
makeEmptySCServerState = atomically $ do
   sockConnectStarted <- newTVar False
   sockIORef <- newEmptyTMVar -- newTVar Nothing -- newIORef Nothing
   listenerIORef <- newEmptyTMVar -- newTVar Nothing -- newIORef Nothing

   availBufIds <- newTVar $ drop 512 $ map BufferId [0..]
   -- these'll be allocated when we connect (and get a clientId):
   availNodeIds <- newTVar $ map (NodeId . ((1 `shiftL` 26) .|.)) [1000..]
   maxBufIds <- newTVar 1024
   syncIds <- newTVar $ drop numberOfSyncIdsToDrop $ map SyncId [0..]
   syncMailboxes <- newTVar $ Map.empty
   serverMessageFunction <- newTVar $ \_ -> return ()
   definedSDs <- newTVar $ Set.empty

   return $ SCServerState
          { _scServerState_socketConnectStarted = sockConnectStarted
          , _scServerState_socket = sockIORef
          , _scServerState_listener = listenerIORef
          , _scServerState_availableBufferIds = availBufIds
          , _scServerState_maxBufIds = maxBufIds
          , _scServerState_availableNodeIds = availNodeIds
          , _scServerState_availableSyncIds = syncIds
          , _scServerState_syncIdMailboxes = syncMailboxes
          , _scServerState_serverMessageFunction = serverMessageFunction
          , _scServerState_definedSDs = definedSDs
          }

-- | If you've started the SC server with a non-default number of buffer ids,
--   (e.g. with the \"-b\" argument), you can reflect that here
-- 
--   Note that the buffer ids start at 512, to not clash with any that
--   another client (e.g. sclang) has allocated
setMaxBufferIds :: Int32 -> IO ()
setMaxBufferIds newMax = atomically $
   writeTVar (_scServerState_maxBufIds scServerState) newMax

getNextAvailable :: (SCServerState -> TVar [a]) -> IO a
getNextAvailable getter =
   getNextAvailables 1 getter >>= \case
      [x] -> return x
      _ -> error "i don't even - 938"

getNextAvailables :: Int -> (SCServerState -> TVar [a]) -> IO [a]
getNextAvailables numToGet getter = do
   let !_ = scServerState
   atomically $ do
      let avail = getter scServerState
      (ns, rest) <- splitAt numToGet <$> readTVar avail
      writeTVar avail rest
      return ns
