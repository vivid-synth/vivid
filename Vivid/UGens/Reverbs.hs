{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Reverbs (
     freeVerb
---   , freeVerb2
   , gVerb
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

-- | Note this is specifically a two-channel UGen
--
--   There are known issues with this! (From SC:)
-- 
--    - \"There is a large CPU spike when the synth is instantiated while all the delay lines are zeroed out.\"
--    - \"Quick changes in roomsize result in zipper noise.\"
--    - \"Changing the roomsize does not work properly! Still trying to look for the bug... (-josh)\"
--
--   Since: vivid-0.4.1
gVerb :: Args '["in"] '["roomSize", "revTime", "damping", "inputBW", "spread", "dryLevel", "earlyRefLevel", "tailLevel", "maxRoomSize"] a => a -> SDBody a [Signal]
gVerb = makePolyUGen 2
   "GVerb" AR
   (Vs::Vs '["in", "roomSize", "revTime", "damping", "inputBW", "spread", "dryLevel", "earlyRefLevel", "tailLevel", "maxRoomSize"])
   (roomSize_ (10::Float), revTime_ (3::Float), damping_ (0.5::Float), inputBW_ (0.5::Float), spread_ (15::Float), dryLevel_ (1::Float), earlyRefLevel_ (0.7::Float), tailLevel_ (0.5::Float), maxRoomSize_ (300::Float))

