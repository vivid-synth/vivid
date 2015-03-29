-- | Internal. Just use "Vivid.SynthDef"

{-# OPTIONS_HADDOCK show-extensions #-}

module Vivid.SynthDef.Types (
     Signal(..)
   , CalculationRate(..)
   , SynthDef(..)
   , SDName(..)
   , SDState
   , UGen(..)
   , UGenName(..)
   , UnaryOp(..)
   , BinaryOp(..)
   ) where

import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Map (Map)

data Signal
   = Constant Float -- constant
   | Param ByteString -- parameter
   | UGOut Int Int32 -- the name of the ugen, and its output #
 deriving (Show, Eq)

-- | Internal representation of Synth Definitions. Usually, use 'Vivid.SynthDef.sd' instead of
--   making these by hand.
-- 
--   This representation (especially '_sdUGens') might change in the future.
data SynthDef = SynthDef {
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

-- | Representation of Unit Generators. You usually won't be creating these
--   by hand, but instead using things from the library in 'Vivid.UGens'
data UGen = UGen {
    _ugenName :: UGenName
   ,_ugenCalculationRate :: CalculationRate
   ,_ugenIns :: [Signal]
   -- The calculation rates of each of the outputs are always the same as the
   -- ugen's calculation rate, so we don't need to represent them:
   ,_ugenNumOuts :: Int
   }
 deriving (Show, Eq)

data UGenName
   = UGName_S ByteString
   | UGName_U UnaryOp
   | UGName_B BinaryOp
 deriving (Show, Eq)

-- The order of these is important for the enum instance:
-- | The rate that a UGen computes at
data CalculationRate
   = IR -- ^ constant value
   | KR -- ^ control rate
   | AR -- ^ audio rate
   | DR -- ^ demand rate
 deriving (Show, Read, Eq, Enum, Ord)

-- | State monad to construct SynthDefs
-- 
--   The SynthDef is an under-construction synth definition
--   The [Int] is the id supply. Its type definitely could change in the future
type SDState = State ([Int], SynthDef)

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
   | Thresh | AMClip | ScaleNeg | Clip2 | Excess
   | Fold2 | Wrap2 | FirstArg
   | RandRange | ExpRandRange | NumBinarySelectors
 deriving (Show, Eq, Ord, Enum)



-- These seem to only be in the SuperCollider source:
--   sc/server/plugins/(Bi|U)naryOpUgens.cpp

-- | Unary signal operations. Many of these have functions so you don't need to
--   use this internal representation (e.g. 'Neg' has 'neg', etc).
-- 
--   This type might not be exposed in the future.
data UnaryOp
   = Neg | Not | IsNil | NotNil | BitNot | Abs | AsFloat | AsInt | Ciel | Floor
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
