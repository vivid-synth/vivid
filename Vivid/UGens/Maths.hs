{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Maths (
     clip
---   , fold -- rename cuz of haskell clashes!
   , inRange
--   , inRect
     -- In Vivid.UGens.Filters:
   -- , integrator
   , leastChange
   , linExp
---   , linLin
---   , modDif
   , mostChange
   , mulAdd
---   , runningMax
---   , runningMin
---   , runningSum
---   , schmidt
     -- In Vivid.UGens.Analysis:
   -- , slope
---   , wrap
   ) where

import Vivid.SynthDef
import Vivid.SynthDef.FromUA
-- import Vivid.UGens.Algebraic
import Vivid.UGens.Args

import qualified Data.ByteString.Char8 as BS8 (pack)
import Data.Proxy

-- | 
--   **Note this has different behavior than 0.1!**
--   \"lo\" is not implied by
--   \"hi\".
-- 
--   If you know you always want \"lo\" to be negative \"hi\" (like -1 to 1)
--   you can use @'biOp' 'Clip2'@
clip :: (Args '["in"] '["lo", "hi"] a) => a -> SDBody a Signal
clip = makeUGen
   "Clip" AR
   (Vs::Vs '["in","lo","hi"])
   (lo_ (0::Float), hi_ (1::Float))

--- fold ::
--- fold =

-- | Returns 1.0 if \"in\" is between \"lo\" and \"hi\" (including
--   equalling "lo" or "hi"). Else 0.0.
-- 
--   Can be audio rate (AR), control rate (KR), or fixed (IR)
-- 
--   Lo and hi default to 0 and 1
inRange :: (Args '["in"] '["lo", "hi"] a) => a -> SDBody a Signal
inRange = makeUGen
   "InRange" AR
   (Vs::Vs '["in", "lo", "hi"])
   (lo_ (0::Float), hi_ (1::Float))

--- -- inRect =

-- | Returns the value of whichever of the 2 signals is changing
--   least
-- 
--   Its default calculation rate is the highest of its 2 inputs
leastChange :: (ToSig s0 as, ToSig s1 as) => s0 -> s1 -> SDBody' as Signal
leastChange = leastOrMostChange "LeastChange"

leastOrMostChange :: (ToSig s0 as, ToSig s1 as) => String -> s0 -> s1 -> SDBody' as Signal
leastOrMostChange sdName s0 s1 = do
   s0' <- toSig s0
   s1' <- toSig s1
   calcRate <- (maximum::Ord a=>[a]->a) <$> sequence (map getCalcRate [s0', s1'])
   addUGen $ UGen (UGName_S . BS8.pack $ sdName) calcRate [s0',s1'] 1

-- | "Converts a linear range of values to an exponential range of values."
-- 
--   Args:
-- 
--   * *in* -  The input signal to convert.
-- 
--   * *srclo* - Lower limit of input range.
-- 
--   * *srchi* - Upper limit of input range.
-- 
--   * *dstlo* - Lower limit of output range.
-- 
--   * *dsthi* - Upper limit of output range.
-- 
--   **"The dstlo and dsthi arguments must be nonzero and have the same sign."**
-- 
--   This will have the same calculation rate as its \"in\" argument
linExp :: Args '["in"] '["srclo", "srchi", "dstlo", "dsthi"] a => a -> SDBody a Signal
linExp as = do
   in' <- as `uaArgVal` (Proxy::Proxy "in")
   getCalcRate in' >>= \case
      AR -> successGraph AR
      KR -> successGraph KR
      _ -> error "linExp: 'in' value must be at AR/KR"
 where
   successGraph calcRate = (flip ($)) as $ makeUGen
      "LinExp" calcRate
      (Vs::Vs '["in", "srclo", "srchi", "dstlo", "dsthi"])
      (srclo_ (0::Float), srchi_ (1::Float), dstlo_ (1::Float), dsthi_ (2::Float))

-- linLin ::
-- linLin =

--- modDif ::
--- modDif =

-- | Opposite of 'leastChange'
mostChange :: (ToSig s0 as, ToSig s1 as) => s0 -> s1 -> SDBody' as Signal
mostChange = leastOrMostChange "MostChange"

mulAdd :: (Args '["in"] '["mul", "add"] a) => a -> SDBody a Signal
mulAdd = makeUGen
   "MulAdd" AR
   (Vs::Vs '["in", "mul", "add"])
   (mul_ (1::Float), add_ (0::Float))

--- runningMax ::
--- runningMax =
--- runningMin ::
--- runningMin =
--- runningSum ::
--- runningSum =
--- schmidt ::
--- schmidt =
--- wrap ::
--- wrap =
