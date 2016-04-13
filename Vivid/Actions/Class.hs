-- | A @VividAction m => m a@ can either be run immediately, be scheduled to run at a
--   precise future time, or be used for non-realtime synthesis.
-- 
--   Note that at the moment VividAction has MonadIO, but this won't be true in
--   upcoming versions (as early as the next release) - so don't get used
--   to it!

{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.Actions.Class (
     VividAction(..)
   , callOSCAndSync
   ) where

import Vivid.OSC
import Vivid.SCServer.State (BufferId, NodeId, SyncId(..))
import Vivid.SynthDef.Types (SynthDef)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8 (pack)
import Control.Monad.IO.Class (MonadIO)

class (Monad m , MonadIO m) => VividAction (m :: * -> *) where

   -- | Send an 'OSC' message to the SuperCollider server
   callOSC :: OSC -> m ()
   callOSC = callBS . encodeOSC

   -- | Send a ByteString to the SuperCollider server.
   --   You usually want to use 'call' instead.
   callBS :: ByteString -> m ()

   -- | Blocks until the server finishes processing commands
   sync :: m ()

   -- | As the user, you probably don't want to use this:
   -- 
   --   Many commands already include a \"sync\" -- e.g.
   --   'makeBuffer' already syncs.
   -- 
   --   When you do want to do an
   --   explicit sync you probably want to use 'sync' instead, or
   --   'callOSCAndSync'
   waitForSync :: SyncId -> m ()

   -- | Wait, in seconds
   wait :: RealFrac n => n -> m ()

   getTime :: m Timestamp

   newBufferId :: m BufferId

   newNodeId :: m NodeId

   newSyncId :: m SyncId

   fork :: m () -> m ()

   -- | Send a synth definition to be loaded on the SC server
   -- 
   --   Note that this is sort of optional -- if you don't call it, it'll be called the first time
   --   you call 'synth' with the SynthDef
   defineSD :: SynthDef a -> m ()

-- | Send an OSC message and wait for it to complete before returning
callOSCAndSync :: VividAction m => OSC -> m ()
callOSCAndSync message = do
   now <- getTime
   sid@(SyncId syncId) <- newSyncId
   callBS $ encodeOSCBundle $
      OSCBundle now [Right message, Right $ OSC (BS8.pack "/sync") [OSC_I syncId]]
   waitForSync sid
