{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Filters (
     module Vivid.UGens.Filters.BEQSuite
   , module Vivid.UGens.Filters.Linear
   , module Vivid.UGens.Filters.Nonlinear
   , module Vivid.UGens.Filters.Pitch

---   , lagUD
---   , lag2UD
---   , lag3UD
   , moogFF
   ) where

import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SynthDef
import Vivid.SynthDef.FromUA
import Vivid.UGens.Args

import Vivid.UGens.Filters.BEQSuite
import Vivid.UGens.Filters.Linear
import Vivid.UGens.Filters.Nonlinear
import Vivid.UGens.Filters.Pitch

--- lagUD ::
--- lagUD =
--- lag2UD ::
--- lag2UD =
--- lag3UD ::
--- lag3UD =

moogFF :: (Args '["in"] '["freq", "gain", "reset"] a) => a -> SDBody a Signal
moogFF = makeUGen
   "MoogFF" AR
   (Vs::Vs '["in", "freq", "gain", "reset"])
   (freq_ (100::Float), gain_ (2::Float), reset_ (0::Float))
