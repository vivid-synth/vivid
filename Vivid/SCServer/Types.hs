{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.SCServer.Types (
     Node(..)
   , shrinkNodeArgs
   , NodeId(..)
   , BufferId(..)
   , SyncId(..)
   , HasNodeId(..)
   ) where

import Data.Int
import GHC.TypeLits
import Vivid.SynthDef.TypesafeArgs


-- | This enforces type safety of the arguments -- e.g. if you have a synthdef
-- 
--   >> let x = sd (3 ::I "foo") bar
--   >> s <- synth x ()
-- 
--   Then this won't typecheck (because "bar" isn't an argument to x):
-- 
--   >> set s (4 ::I "bar")
-- 
--   Note that if you don't want this type safety, you can e.g.
-- 
--   >> Node n <- synth foo ()
--   >> setG n (0.1 ::I "vol")
-- 
--   Or:
-- 
--   >> ns <- mapM (flip synth ()) [foo, bar, baz]
--   >> map (setG (0::I "asdf") . unNode) ns
-- 
--   Or:
-- 
--   >> n <- synthG foo ()
-- 
--   (You also may want to look at 'shrinkNodeArgs' if you want to construct a list
--   which has synthdefs or nodes of different types)
data Node (args :: [Symbol])
   = Node { unNode :: NodeId }

-- | So let's say you have a node:
-- 
--   > foo :: Node '["amp", "freq", "phase"]
-- 
--   and you want to add it to a list of nodes:
-- 
--   > ns :: [Node '["freq", "phase"]]
-- 
--   If you don't plan on setting the \"amp\" argument, you can \"shrink\" to
--   the compatible arguments:
-- 
--   > ns' = shrinkNodeArgs foo : ns
-- 
--   (The same thing exists for SynthDefs -- 'Vivid.SynthDef.shrinkSDArgs')
shrinkNodeArgs :: (Subset new old) => Node old -> Node new
shrinkNodeArgs (Node nId) = Node nId

newtype NodeId
      = NodeId { _unNodeId :: Int32 }
   deriving (Show, Eq, Ord, Read)

class HasNodeId a where
   getNodeId :: a -> NodeId

instance HasNodeId NodeId where
   getNodeId n = n

instance HasNodeId (Node a) where
   getNodeId (Node n) = n

newtype BufferId
      = BufferId { _unBufferId :: Int32 }
   deriving (Show, Eq, Ord, Read)

newtype SyncId
      = SyncId Int32
   deriving (Show, Read, Eq, Ord)
