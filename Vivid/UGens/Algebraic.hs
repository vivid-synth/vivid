-- | The fixities of the operators ("~+", "~*", "~<", etc) are the same as those of
--   their non-'~' equivalents (+, *, < etc)
--
--   (So you can e.g. multiply then add without parens!)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, NoMonoLocalBinds #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Algebraic (
     (~+)
   , (~-)
   , (~*)
   , (~**)
   , (~/)

{- -- removed these to be compatible with tidal, which also uses some of these:
   , (~>)
   , (~>=)
   , (~<)
   , (~<=)
-}
   , binaryOp
   , biOp
   , unaryOp
   , uOp
   , midiCPS
   , cpsMIDI
   , abs'
   , neg
   , tanh'
   , clip2
   , xor
   ) where

import Vivid.SynthDef
-- import Vivid.UGens.Args

import Prelude

infixl 7 ~*
-- | Multiply signals
(~*) :: (ToSig i0 a, ToSig i1 a) => i0 -> i1 -> SDBody' a Signal
(~*) = binaryOp Mul

infixr 8 ~**
-- | Exponentiation of signals
(~**) :: (ToSig i0 a, ToSig i1 a) => i0 -> i1 -> SDBody' a Signal
(~**) = binaryOp Pow

infixl 6 ~+
-- | Add signals
(~+) :: (ToSig i0 a, ToSig i1 a) => i0 -> i1 -> SDBody' a Signal
(~+) = binaryOp Add

infixl 7 ~/
-- | Divide signals
(~/) :: (ToSig i0 a, ToSig i1 a) => i0 -> i1 -> SDBody' a Signal
(~/) = binaryOp FDiv

{-
infix 4 ~>
-- | Test signals for left greater than right
(~>) :: (ToSig i0 a, ToSig i1 a) => i0 -> i1 -> SDBody' a Signal
(~>) = binaryOp Gt

infix 4 ~>=
-- | Test signals for left greater than or equal to right
(~>=) :: (ToSig i0 a, ToSig i1 a) => i0 -> i1 -> SDBody' a Signal
(~>=) = binaryOp Ge

infix 4 ~<
-- | Test signals for left less than right
(~<) :: (ToSig i0 a, ToSig i1 a) => i0 -> i1 -> SDBody' a Signal
(~<) = binaryOp Lt

infix 4 ~<=
-- | Test signals for left less than or equal to right
(~<=) :: (ToSig i0 a, ToSig i1 a) => i0 -> i1 -> SDBody' a Signal
(~<=) = binaryOp Le
-}

infixl 6 ~-
-- | Subtract signals
(~-) :: (ToSig i0 a, ToSig i1 a) => i0 -> i1 -> SDBody' a Signal
(~-) = binaryOp Sub

-- | Build your own!
-- 
--   The calculation rate of the result is the larger (more frequent) of the 2 input
--   signals
--   (So you shouldn't need to use "?" very much!)
binaryOp :: (ToSig s0 a, ToSig s1 a) => BinaryOp -> s0 -> s1 -> SDBody' a Signal
binaryOp theBiOp s0 s1 = do
   s0' <- toSig s0
   s1' <-  toSig s1
   calcRate <- max <$> getCalcRate s0' <*> getCalcRate s1'
   let sigs = [s0', s1']
   addUGen $ UGen (UGName_B theBiOp) calcRate sigs 1

-- | Alias of 'binaryOp'. Shorter, fer livecodin
biOp :: (ToSig s0 a, ToSig s1 a) => BinaryOp -> s0 -> s1 -> SDBody' a Signal
biOp = binaryOp

-- | Build your own, from 'UnaryOp's
unaryOp :: (ToSig sig a) => UnaryOp -> sig -> SDBody' a Signal
unaryOp theUOp sig = do
   sig' <- toSig sig
   calcRate <- getCalcRate sig'
   addUGen $ UGen (UGName_U theUOp) calcRate [sig'] 1

-- | Alias of 'unaryOp'
uOp :: (ToSig sig a) => UnaryOp -> sig -> SDBody' a Signal
uOp = unaryOp

-- | Convert from a midi note number (0-127, each representing a musical half step) to a
--   frequency in hz (cycles per second)
midiCPS :: (ToSig i a) => i -> SDBody' a Signal
midiCPS = unaryOp MIDICPS

-- | Inverse of 'midiCPS'
cpsMIDI :: (ToSig i a) => i -> SDBody' a Signal
cpsMIDI = unaryOp CPSMIDI



-- | The prime is to not conflict with \"abs\" in the prelude. May just use
--   \"uOp Abs\" in the future
abs' :: (ToSig i a) => i -> SDBody' a Signal
abs' = unaryOp Abs

neg :: (ToSig i a) => i -> SDBody' a Signal
neg = unaryOp Neg

-- | The prime, like 'abs'', is to not conflict with a prelude definition.
-- 
--   Remember you can always just use:
-- 
--   > uOp TanH
tanh' :: ToSig i a => i -> SDBody' a Signal
tanh' i = uOp TanH i

-- | Like 'Vivid.UGens.Maths.clip' but the lo value is always negative the hi value
clip2 :: (ToSig s0 a, ToSig s1 a) => s0 -> s1 -> SDBody' a Signal
clip2 = biOp Clip2

-- | Bitwise xor. Short for @biOp BitXor@
xor :: (ToSig s0 a, ToSig s1 a) => s0 -> s1 -> SDBody' a Signal
xor = biOp BitXor
