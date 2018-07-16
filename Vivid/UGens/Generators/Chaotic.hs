{-# LANGUAGE DataKinds #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Generators.Chaotic (
     cuspL
   , cuspN
---   , fbSineC
---   , fbSineL
---   , fbSineN
---   , gbmanL
---   , gbmanN
---   , henonC
---   , henonL
---   , henonN
---   , latoocarfianC
---   , latoocarfianL
---   , latoocarfianN
   , linCongC
   , linCongL
   , linCongN
---   , logistic
---   , lorenzL
---   , quadC
---   , quadL
---   , quadN
---   , sinOscFB
---   , standardL
---   , standardN
   ) where

import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.UGens.Args
import Vivid.SynthDef
import Vivid.SynthDef.FromUA

-- | "A linear-interpolating sound generator based on the difference equation:
-- 
--   x[n+1] = a - b * sqrt(abs(x[n]))"
-- 
--   Only has an AR instance
cuspL :: (Args '[] '["freq", "a", "b", "xi"] a) => a -> SDBody a Signal
cuspL = makeUGen
   "CuspL" AR
   (Vs::Vs '["freq", "a", "b", "xi"])
   (freq_ (22050::Float), a_ (1::Float), b_ (1.9::Float), xi_ (0::Float))

-- | "A non-interpolating sound generator based on the difference equation:
-- 
--    x[n+1] = a - b * sqrt(abs(x[n]))"
-- 
--    Only has an AR instance.
cuspN :: (Args '[] '["freq", "a", "b", "xi"] a) => a -> SDBody a Signal
cuspN = makeUGen
   "CuspN" AR
   (Vs::Vs '["freq", "a", "b", "xi"])
   (freq_ (22050::Float), a_ (1::Float), b_ (1.9::Float), xi_ (0::Float))

--- fbSineC ::
--- fbSineC =
--- fbSineL ::
--- fbSineL =
--- fbSineN ::
--- fbSineN =
--- gbmanL ::
--- gbmanL =
--- gbmanN ::
--- gbmanN =
--- henonC ::
--- henonC =
--- henonL ::
--- henonL =
--- henonN ::
--- henonN =
--- latoocarfianC ::
--- latoocarfianC =
--- latoocarfianL ::
--- latoocarfianL =
--- latoocarfianN ::
--- latoocarfianN =

-- | "A cubic-interpolating sound generator based on the difference equation:
-- 
--   x[n+1] = (a * x[n] + c) % m
-- 
--   The output signal is automatically scaled to a range of [-1, 1]."
--  
--   Only has a "AR" method
linCongC :: (Args '[] '["freq", "a", "c", "m", "xi"] a) => a -> SDBody a Signal
linCongC = makeUGen
   "LinCongC" AR
   (Vs::Vs '["freq", "a", "c", "m", "xi"])
   (freq_ (22050::Float), a_ (1.1::Float), c_ (0.13::Float), m_ (1::Float), xi_ (0::Float))

-- | "A linear-interpolating sound generator based on the difference equation:
-- 
--   x[n+1] = (a * x[n] + c) % m
-- 
--   The output signal is automatically scaled to a range of [-1, 1]."
-- 
--   Only has a "AR" method
linCongL :: (Args '[] '["freq", "a", "c", "m", "xi"] a) => a -> SDBody a Signal
linCongL = makeUGen
   "LinCongL" AR
   (Vs::Vs '["freq", "a", "c", "m", "xi"])
   (freq_ (22050::Float), a_ (1.1::Float), c_ (0.13::Float), m_ (1::Float), xi_ (0::Float))

-- | "A non-interpolating sound generator based on the difference equation:
-- 
--   x[n+1] = (a * x[n] + c) % m
-- 
--   The output signal is automatically scaled to a range of [-1, 1]."
-- 
--   Only has a "AR" method
linCongN :: (Args '[] '["freq", "a", "c", "m", "xi"] a) => a -> SDBody a Signal
linCongN = makeUGen
   "LinCongN" AR
   (Vs::Vs '["freq", "a", "c", "m", "xi"])
   (freq_ (22050::Float), a_ (1.1::Float), c_ (0.13::Float), m_ (1::Float), xi_ (0::Float))

--- logistic ::
--- logistic =
--- lorenzL ::
--- lorenzL =
--- quadC ::
--- quadC =
--- quadL ::
--- quadL =
--- quadN ::
--- quadN =
--- sinOscFB ::
--- sinOscFB =
--- standardL ::
--- standardL =
--- standardN ::
--- standardN =
