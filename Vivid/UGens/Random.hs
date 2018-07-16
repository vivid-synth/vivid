-- | For if you want a SynthDef where each Synth instance has a new random number.
-- 
--   Creates a random value between \"lo\" and \"hi\". The value never changes in
--   the synth.
-- 
--   These compute at "IR"

{-# LANGUAGE DataKinds #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Random (
      -- In Vivid.UGens.Demand:
   --   dshuf
   -- , dwrand
     expRand
     -- In Vivid.UGens.Filters:
   -- , hasher
---   , iRand
   , linRand
---   , nRand
   , rand
     -- In Vivid.UGens.Generators.Stochastic:
   -- , randID
   -- , randSeed
     -- in UGens.Triggers:
   -- , tChoose
   -- , tExpRand
   -- , tIRand
   -- , tRand
   -- , tWChoose
   -- , tWIndex
   ) where

import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SynthDef
import Vivid.SynthDef.FromUA
import Vivid.UGens.Args

expRand :: (Args '[] '["lo","hi"] a) => a -> SDBody a Signal
expRand = makeUGen
   "ExpRand" IR
   (Vs::Vs '["lo", "hi"])
   (lo_ (0::Float), hi_ (1::Float))

--- iRand ::
--- iRand =

linRand :: (Args '[] '["lo","hi","minmax"] a) => a -> SDBody a Signal
linRand = makeUGen
   "LinRand" IR
   (Vs::Vs '["lo", "hi", "minmax"])
   (lo_ (0::Float), hi_ (1::Float), minmax_ (0::Float))

--- nRand ::
--- nRand =

rand :: (Args '[] '["lo","hi"] a) => a -> SDBody a Signal
rand = makeUGen
   "Rand" IR
   (Vs::Vs '["lo", "hi"])
   (lo_ (0::Float), hi_ (1::Float))
