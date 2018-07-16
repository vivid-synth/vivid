{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Generators.Deterministic (
---     blip
---   , cosc
---   , dynKlang
     -- In Vivid.UGens.Filters:
   -- , dynKlank
     fSinOsc
   , formant
   , impulse
---   , klang
     -- In Vivid.UGens.Filters:
   -- , klank
   , lfCub
   , lfGauss
   , lfPar
   , lfPulse
   , lfSaw
   , lfTri
---   , osc
---   , oscN
---   , pmOSC
---   , pSinGrain
   , pulse
   , saw
   , sinOsc
     -- In Vivid.UGens.Generators.Chaotic:
   -- , sinOscFB
   , syncSaw
   , varSaw
     -- In Vivid.UGens.Filters:
   -- , vibrato
---   , vOsc
---   , vOsc3
   ) where

import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SynthDef
import Vivid.SynthDef.FromUA
import Vivid.UGens.Args

--- blip ::
--- blip =
--- cosc ::
--- cosc =
--- dynKlang ::
--- dynKlang =

fSinOsc :: (Args '["freq"] '["phase"] a) => a -> SDBody a Signal
fSinOsc = makeUGen
   "FSinOsc" AR
   (Vs::Vs '["freq", "phase"])
   (phase_ (0::Float))

-- | Only runs at audio rate. All arguments must be at control rate or constant.
--   \"bwFreq\" must be greater than or equal to \"fundFreq\".
formant :: Args '["fundFreq", "formFreq", "bwFreq"] '[] a => a -> SDBody a Signal
formant = makeUGen
   "Formant" AR
   (Vs::Vs '["fundFreq", "formFreq", "bwFreq"])
   NoDefaults

impulse :: (Args '["freq"] '["phase"] a) => a -> SDBody a Signal
impulse = makeUGen
   "Impulse" AR
   (Vs::Vs '["freq", "phase"])
   (phase_ (0::Float))

--- klang ::
--- klang =

lfCub :: (Args '["freq"] '["iphase"] a) => a -> SDBody a Signal
lfCub = makeUGen
   "LFCub" AR
   (Vs::Vs '["freq", "iphase"])
   (iphase_ (0::Float))

lfGauss :: (Args '[] '["duration", "width", "iphase", "loop", "doneAction"] a) => a -> SDBody a Signal
lfGauss = makeUGen
   "LFGauss" AR
   (Vs::Vs '["duration", "width", "iphase", "loop", "doneAction"])
   (duration_ (1::Float), width_ (0.1::Float), iphase_ (0::Float), loop_ (1::Float), doneAction_ (0::Float))

lfPar :: (Args '["freq"] '["iphase"] a) => a -> SDBody a Signal
lfPar = makeUGen
   "LFPar" AR
   (Vs::Vs '["freq", "iphase"])
   (iphase_ (0::Float))

lfPulse :: (Args '["freq"] '["iphase", "width"] a) => a -> SDBody a Signal
lfPulse = makeUGen
   "LFPulse" AR
   (Vs::Vs '["freq", "iphase", "width"])
   (iphase_ (0::Float), width_ (0.5::Float))

-- | \"A non-band-limited sawtooth oscillator. Output ranges from -1 to +1.\"
lfSaw :: (Args '["freq"] '["iphase"] a) => a -> SDBody a Signal
lfSaw = makeUGen
   "LFSaw" AR
   (Vs::Vs '["freq", "iphase"])
   (iphase_ (0::Float))

-- | \"A non-band-limited triangle oscillator. Output ranges from -1 to +1.\"
lfTri :: (Args '["freq"] '["iphase"] a) => a -> SDBody a Signal
lfTri = makeUGen
   "LFTri" AR
   (Vs::Vs '["freq", "iphase"])
   (iphase_ (0::Float))

--- osc ::
--- osc =
--- oscN ::
--- oscN =
--- pmOSC ::
--- pmOSC =
--- pSinGrain ::
--- pSinGrain =

pulse :: (Args '["freq"] '["width"] a) => a -> SDBody a Signal
pulse = makeUGen
   "Pulse" AR
   (Vs::Vs '["freq","width"])
   (width_ (0.5::Float))

saw :: (Args '["freq"] '[] a) => a -> SDBody a Signal
saw = makeUGen
   "Saw" AR
   (Vs::Vs '["freq"])
   NoDefaults

-- | Sine wave
sinOsc :: (Args '["freq"] '["phase"] a) => a -> SDBody a Signal
sinOsc = makeUGen
   "SinOsc" AR
   (Vs::Vs '["freq","phase"])
   (phase_ (0::Float))

syncSaw :: (Args '["syncFreq", "sawFreq"] '[] a) => a -> SDBody a Signal
syncSaw = makeUGen
   "SyncSaw" AR
   (Vs::Vs '["syncFreq", "sawFreq"])
   NoDefaults

-- | Width is "duty cycle from 0 to 1"
varSaw :: (Args '["freq"] '["iphase", "width"] a) => a -> SDBody a Signal
varSaw = makeUGen
   "VarSaw" AR
   (Vs::Vs '["freq", "iphase", "width"])
   (iphase_ (0::Float), width_ (0.5::Float))

--- vOsc ::
--- vOsc =
--- vOsc3 ::
--- vOsc3 =
