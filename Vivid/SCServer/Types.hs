{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.SCServer.Types (

   -- Types
     Synth(..)

   -- Functions
   , shrinkSynthArgs

   -- Classes
   , IsNode(..)
   , SynthOrNodeId
   , IsGroup
   ) where

import Data.Int
import GHC.TypeLits
import Vivid.SC.Server.Types
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
--   >> Synth n <- synth foo ()
--   >> setG n (0.1 ::I "vol")
-- 
--   Or:
-- 
--   >> ns <- mapM (flip synth ()) [foo, bar, baz]
--   >> map (setG (0::I "asdf") . unSynth) ns
-- 
--   Or:
-- 
--   >> n <- synthG foo ()
-- 
--   (You also may want to look at 'shrinkSynthArgs' if you want to construct a list
--   which has synthdefs or nodes of different types)
newtype Synth (args :: [Symbol]) = Synth { _unSynth :: NodeId }
 deriving (Show, Read, Eq, Ord)

-- | So let's say you have a node:
-- 
--   > foo :: Synth '["amp", "freq", "phase"]
-- 
--   and you want to add it to a list of nodes:
-- 
--   > ns :: [Synth '["freq", "phase"]]
-- 
--   If you don't plan on setting the \"amp\" argument, you can \"shrink\" to
--   the compatible arguments:
-- 
--   > ns' = shrinkSynthArgs foo : ns
-- 
--   (The same thing exists for SynthDefs -- 'Vivid.SynthDef.shrinkSDArgs')
shrinkSynthArgs :: (Subset new old) => Synth old -> Synth new
shrinkSynthArgs (Synth nId) = Synth nId

class IsNode a where getNodeId :: a -> NodeId

instance IsNode NodeId where getNodeId n = n
instance IsNode (Synth a) where getNodeId (Synth n) = n
instance IsNode Group where getNodeId (Group n) = n
instance IsNode ParGroup where getNodeId (ParGroup n) = n

-- | For gradually-typed 'free'
class IsNode a => SynthOrNodeId a
instance SynthOrNodeId (Synth x)
instance SynthOrNodeId NodeId

-- | 'Group' and 'ParGroup'
class IsNode g => IsGroup g
instance IsGroup Group
instance IsGroup ParGroup
