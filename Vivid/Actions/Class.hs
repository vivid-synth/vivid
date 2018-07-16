-- | A @VividAction m => m a@ can either be run immediately, be scheduled to run at a
--   precise future time, or be used for non-realtime synthesis.
-- 
--   Note that at the moment VividAction has MonadIO, but this won't be true in
--   upcoming versions (as early as the next release) - so don't get used
--   to it!

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.Actions.Class (
     VividAction(..)
   -- , VA
   , callOSCAndSync
   , oscWSync
   ) where

import Vivid.SC.Server.Types (BufferId, NodeId, SyncId(..))
import qualified Vivid.SC.Server.Commands as SCCmd

import Vivid.OSC
import Vivid.SynthDef.Types (SynthDef)

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import Data.Int

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
   wait :: Real n => n -> m ()

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
   syncId <- newSyncId
   callBS $ encodeOSCBundle $
      OSCBundle now [Right message, Right $ SCCmd.sync syncId]
   waitForSync syncId

-- | 
-- 
--   Maybe can dedupe with 'callOSCAndSync'
oscWSync :: VividAction m => (SyncId -> m ()) -> m ()
oscWSync actionFromId = do
   syncId <- newSyncId
   actionFromId syncId
   waitForSync syncId


-- type VA x = forall m. VividAction m => m x

