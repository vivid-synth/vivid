-- | Internal. Just use "Vivid.SynthDef"

{-# OPTIONS_HADDOCK show-extensions #-}
-- {-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs, NoMonoLocalBinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.SynthDef.Types (
     Signal(..)
   , CalculationRate(..)
   , SynthDef(..)
   , SDName(..)
   , SDBody'
   , zoomSDBody
   , zoomSynthDef
   , UGen(..)
   , UGenName(..)
   , UnaryOp(..)
   , BinaryOp(..)
   , module Vivid.SynthDef.TypesafeArgs
   ) where

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

-- The order of these is important for the enum instance:
-- | The rate that a UGen computes at
data CalculationRate
   = IR -- ^ constant value
   | KR -- ^ control rate
   | AR -- ^ audio rate
   | DR -- ^ demand rate
 deriving (Show, Read, Eq, Enum, Ord)

-- instance Hashable CalculationRate

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


-- | Binary signal operations. For the simple ones (like 'Add', 'Mul', etc.),
--   there are functions (like 'Vivid.UGens.~+', 'Vivid.UGens.~*', etc.)
--   that wrap them up so you
--   don't have to make a ugen for them yourself.
-- 
--   In the future these may not be exported -- we'll just have functions for
--   all of them.
data BinaryOp
   = Add | Sub | Mul 
   | IDiv -- ^ Integer division
   | FDiv -- ^ Float division
   | Mod | Eq | Ne | Lt | Gt | Le | Ge
   | Min | Max | BitAnd | BitOr | BitXor | Lcm | Gcd | Round | RoundUp | Trunc
   | Atan2 | Hypot | Hypotx | Pow | ShiftLeft | ShiftRight | UnsignedShift | Fill
   -- comments come from SC source:
   | Ring1 -- ^ a * (b + 1) == a * b + a
   | Ring2 -- ^ a * b + a + b
   | Ring3 -- ^ a * a * b
   | Ring4 -- ^ a * a * b - a * b * b
   | DifSqr -- ^ a * a - b * b
   | SumSqr -- ^ a * a + b * b
   | SqrSum -- ^ (a + b) ^ 2
   | SqrDif -- ^ (a - b) ^ 2
   | AbsDif -- ^ abs(a - b)
   | Thresh
   | AMClip
   | ScaleNeg
   | Clip2 -- Like 'Vivid.UGens.Maths.clip' but the min value is always the negative of the max value
   | Excess
   | Fold2 | Wrap2 | FirstArg
   | RandRange | ExpRandRange | NumBinarySelectors
 deriving (Show, Eq, Ord, Enum)

-- instance Hashable BinaryOp

-- These seem to only be in the SuperCollider source:
--   sc/server/plugins/(Bi|U)naryOpUgens.cpp

-- | Unary signal operations. Many of these have functions so you don't need to
--   use this internal representation (e.g. 'Neg' has 'neg', etc).
-- 
--   This type might not be exposed in the future.
data UnaryOp
   = Neg | Not | IsNil | NotNil
   | BitNot -- ^ There's a bug in some SC versions where .bitNot isn't implemented
            --   correctly. Vivid backfills it with a fix, so you can use BitNot with
            --   any SC version
   | Abs | AsFloat | AsInt | Ciel | Floor
   | Frac | Sign | Squared | Cubed | Sqrt | Exp | Recip | MIDICPS | CPSMIDI
   | MIDIRatio | RatioMIDI
    -- dbamp and ampdb: converts betw db and "amp" so that e.g. -inf db == 0 amp
    -- dunno how the other scaling works
   | DbAmp | AmpDb
   | OctCPS | CPSOct | Log | Log2 | Log10
   | Sin | Cos | Tan | ArcSin | ArcCos | ArcTan | SinH | CosH | TanH
   | Rand | Rand2 | LinRand | BiLinRand | Sum3Rand
   | Distort | SoftClip | Coin | DigitValue
   | Silence | Thru | RectWindow | HanWindow | WelchWindow | TriWindow | Ramp
   | SCurve | NumUnarySelectors
 deriving (Show, Eq, Ord, Enum)

-- instance Hashable UnaryOp
