{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Reverbs (
     freeVerb
---   , freeVerb2
---   , gVerb
   ) where

import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SynthDef
import Vivid.SynthDef.FromUA
import Vivid.UGens.Args

-- | \"mix\", \"room\", and \"damp\" params range from 0 to1
freeVerb :: (Args '["in"] '["mix", "room", "damp"] a) => a -> SDBody a Signal
freeVerb = makeUGen
   "FreeVerb" AR
   (Vs::Vs '["in", "mix", "room", "damp"])
   (mix_ (1/3::Float), room_ (0.5::Float), damp_ (0.5::Float))

--- freeVerb2 ::
--- freeVerb2 =

-- | there are known issues with this! look in scide:
--- gVerb ::
--- gVerb =
