{-# LANGUAGE DataKinds #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Filters.Nonlinear (
     ball
     -- In Vivid.UGens.Filters.Pitch:
   -- , freqShift
   , hasher
---   , hilbert
---   , hilbertFIR
   , mantissaMask
   , median
   , slew
   , spring
   , tBall
   ) where

import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SynthDef ({- SDBody, -} Signal)
import Vivid.UGens.Args
import Vivid.SynthDef.FromUA
import Vivid.SynthDef.TypesafeArgs

ball :: (Args '["in"] '["g", "damp", "friction"] a) => a -> SDBody a Signal
ball = makeUGen
   "Ball" AR
   (Vs::Vs '["in", "g", "damp", "friction"])
   (g_ (1::Float), damp_ (0::Float), friction_ (0.01::Float))

hasher :: (Args '["in"] '[] a) => a -> SDBody a Signal
hasher = makeUGen
   "Hasher" AR
   (Vs::Vs '["in"])
   NoDefaults

-- returns 2 channels -- also only has an AR instance
--- hilbert ::
--- hilbert =
--see "hilbert":
--- hilbertFIR ::
--- hilbertFIR =

mantissaMask :: (Args '["in"] '["bits"] a) => a -> SDBody a Signal
mantissaMask = makeUGen
   "MantissaMask" AR
   (Vs::Vs '["in", "bits"])
   (bits_ (3::Float))

median :: (Args '["in"] '["length"] a) => a -> SDBody a Signal
median = makeUGen
   "Median" AR
   (Vs::Vs '["length", "in"])
   (length_ (3::Float))

slew :: (Args '["in"] '["up", "dn"] a) => a -> SDBody a Signal
slew = makeUGen
   "Slew" AR
   (Vs::Vs '["in", "up", "dn"])
   (up_ (1::Float), dn_ (1::Float))

spring :: (Args '["in"] '["spring", "damp"] a) => a -> SDBody a Signal
spring = makeUGen
   "Spring" AR
   (Vs::Vs '["in", "spring", "damp"])
   (spring_ (1::Float), damp_ (0::Float))

tBall :: (Args '["in"] '["g", "damp", "friction"] a) => a -> SDBody a Signal
tBall = makeUGen
   "TBall" AR
   (Vs::Vs '["in", "g", "damp", "friction"])
   (g_ (10::Float), damp_ (0::Float), friction_ (0.01::Float))
