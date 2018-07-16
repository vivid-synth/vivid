-- | Internal. Just use "Vivid.SynthDef"

{-# OPTIONS_HADDOCK show-extensions #-}
-- {-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE GADTs, NoMonoLocalBinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, NoMonoLocalBinds #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

-- TODO: rename so 
module Vivid.SynthDef.Types (
     Signal(..)
   , SynthDef(..)
   , SDName(..)
   , SDBody'
   , zoomSDBody
   , zoomSynthDef
   , UGen(..)
   , UGenName(..)
   , module Vivid.SynthDef.TypesafeArgs
   ) where

import Vivid.SC.SynthDef.Types (CalculationRate(..), UnaryOp(..), BinaryOp(..))
import Vivid.SynthDef.TypesafeArgs -- (VarSet(..), Subset, Parameter)

import Control.Monad.State (State, get, runState, put)
import Data.ByteString (ByteString)
-- import Data.Hashable
import Data.Int (Int32)
-- import qualified Data.Map as Map
import Data.Map (Map)
-- import Data.Monoid
import GHC.TypeLits
import Prelude

data Signal
   = Constant Float
   | Param ByteString
   | UGOut Int Int32  -- the name of the ugen, and its output #
  deriving (Show, Eq)

-- instance Hashable Signal

-- | Internal representation of Synth Definitions. Usually, use 'Vivid.SynthDef.sd' instead of
--   making these by hand.
-- 
--   This representation (especially '_sdUGens') might change in the future.
data SynthDef (args :: [Symbol]) = SynthDef {
    _sdName :: SDName
   ,_sdParams :: [(ByteString, Float)]
   ,_sdUGens :: Map Int UGen
   -- ignoring variants
   }
 deriving (Show)

data SDName
   = SDName_Named ByteString
   | SDName_Hash
 deriving (Show, Eq, Read, Ord)

-- instance Hashable SDName

-- | Representation of Unit Generators. You usually won't be creating these
--   by hand, but instead using things from the library in 'Vivid.UGens'
data UGen
   = UGen {
    _ugenName :: UGenName
   ,_ugenCalculationRate :: CalculationRate
   ,_ugenIns :: [Signal]
   -- The calculation rates of each of the outputs are always the same as the
   -- ugen's calculation rate, so we don't need to represent them:
   ,_ugenNumOuts :: Int
   }
  deriving (Show, Eq)

-- instance Hashable UGen

data UGenName
   = UGName_S ByteString
   | UGName_U UnaryOp
   | UGName_B BinaryOp
 deriving (Show, Eq)

-- instance Hashable UGenName

-- | State monad to construct SynthDefs
-- 
--   The SynthDef is an under-construction synth definition
--   The [Int] is the id supply. Its type definitely could change in the future
type SDBody' (args :: [Symbol])
   = State ([Int], SynthDef args, VarSet args)

zoomSynthDef :: (Subset a b) => SynthDef a -> SynthDef b
zoomSynthDef (SynthDef a b c) = SynthDef a b c

-- | Given
-- 
--   > good0 :: SDBody '["2"] ()
--   > good0 = return ()
-- 
--   > good1 :: SDBody '["3","1","3","1"] ()
--   > good1 = return ()
-- 
--   > bad0 :: SDBody '["bwahaha"] ()
--   > bad0 = return ()
-- 
--   > outer :: SDBody '[ "1", "2", "3"]()
--   > outer = do
--   >    zoomSDBody good0 -- works
--   >    zoomSDBody good1 -- works
--   >    -- zoomSDBody bad0 -- doesn't work - great!
zoomSDBody :: (Subset inner outer) => SDBody' inner a -> SDBody' outer a
zoomSDBody x = do
   (initA,initB,_) <- get
   -- We call this "cheat" cause it actually goes from outer
   -- to inner -- it's only safe cause we already restricted
   -- the input to this outer function ('zoomSDBody'):
   let cheatSD :: SynthDef a -> SynthDef b
       cheatSD (SynthDef a b c) = SynthDef a b c
   let (val,(a,b,_)) = runState x (initA, cheatSD initB,VarSet)
   put (a, zoomSynthDef b, VarSet)
   return val


