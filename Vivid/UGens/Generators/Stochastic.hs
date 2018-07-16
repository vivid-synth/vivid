{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Generators.Stochastic (
     brownNoise
   , clipNoise
---   , coinGate
---   , crackle
   , dust
   , dust2
---   , gendy1
---   , gendy2
---   , gendy3
   , grayNoise
   , lfClipNoise
   , lfdClipNoise
   , lfdNoise0
   , lfdNoise1
   , lfdNoise3
   , lfNoise0
   , lfNoise1
   , lfNoise2
   , pinkNoise
---   , randID
---   , randSeed
     -- In Vivid.UGens.Filters.Pitch:
   -- , vibrato
   , whiteNoise
   ) where

import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SynthDef
import Vivid.SynthDef.FromUA
import Vivid.UGens.Args

-- | \"Generates noise whose spectrum falls off in power by 6 dB per octave.\"
brownNoise :: SDBody' a Signal
brownNoise = addUGen $ UGen (UGName_S "BrownNoise") AR [] 1

-- | \"Generates noise whose values are either -1 or 1. This produces the maximum energy for the least peak to peak amplitude.\"
clipNoise :: SDBody' a Signal
clipNoise = addUGen $ UGen (UGName_S "ClipNoise") AR [] 1

--- coinGate ::
--- coinGate =
--- crackle ::
--- crackle =

-- | \"Generates random impulses from -1 to +1.\"
dust :: (Args '["density"] '[] a) => a -> SDBody a Signal
dust = makeUGen
   "Dust" AR
   (Vs::Vs '["density"])
   NoDefaults

-- | \"Generates random impulses from -1 to +1.\"
dust2 :: (Args '["density"] '[] a) => a -> SDBody a Signal
dust2 = makeUGen
   "Dust2" AR
   (Vs::Vs '["density"])
   NoDefaults

--- gendy1 ::
--- gendy1 =
--- gendy2 ::
--- gendy2 =
--- gendy3 ::
--- gendy3 =

-- | \"Generates noise which results from flipping random bits in a word. This type of noise has a high RMS level relative to its peak to peak level. The spectrum is emphasized towards lower frequencies.\"
grayNoise :: SDBody' a Signal
grayNoise = addUGen $ UGen (UGName_S "GrayNoise") AR [] 1

-- | E.g.
-- 
--   > play $ 0.1 ~* lfClipNoise (freq_ $ xLine (start_ 1e3, end_ 1e4, secs_ 10))
lfClipNoise :: (Args '[] '["freq"] a) => a -> SDBody a Signal
lfClipNoise = makeUGen
   "LFClipNoise" AR
   (Vs::Vs '["freq"])
   (freq_ (500::Float))

-- | \"Like LFClipNoise, it generates the values -1 or +1 at a rate given by the freq argument, with two differences:
--      \" - no time quantization
--      \" - fast recovery from low freq values
--   \" If you don't need very high or very low freqs, or use fixed freqs, LFDClipNoise is more efficient."
lfdClipNoise :: (Args '[] '["freq"] a) => a -> SDBody a Signal
lfdClipNoise = makeUGen
   "LFDClipNoise" AR
   (Vs::Vs '["freq"])
   (freq_ (500::Float))

lfdNoise0 :: (Args '[] '["freq"] a) => a -> SDBody a Signal
lfdNoise0 = makeUGen
   "LFDNoise0" AR
   (Vs::Vs '["freq"])
   (freq_ (500::Float))

lfdNoise1 :: (Args '[] '["freq"] a) => a -> SDBody a Signal
lfdNoise1 = makeUGen
   "LFDNoise1" AR
   (Vs::Vs '["freq"])
   (freq_ (500::Float))

lfdNoise3 :: (Args '[] '["freq"] a) => a -> SDBody a Signal
lfdNoise3 = makeUGen
   "LFDNoise3" AR
   (Vs::Vs '["freq"])
   (freq_ (500::Float))

-- | Freq is \"approximate rate at which to generate random values\"
lfNoise0 :: (Args '[] '["freq"] a) => a -> SDBody a Signal
lfNoise0 = makeUGen
   "LFNoise0" AR
   (Vs::Vs '["freq"])
   (freq_ (500::Float))

-- | Freq is \"approximate rate at which to generate random values\"
lfNoise1 :: (Args '[] '["freq"] a) => a -> SDBody a Signal
lfNoise1 = makeUGen
   "LFNoise1" AR
   (Vs::Vs '["freq"])
   (freq_ (500::Float))

-- | Freq is \"approximate rate at which to generate random values\"
lfNoise2 :: (Args '[] '["freq"] a) => a -> SDBody a Signal
lfNoise2 = makeUGen
   "LFNoise2" AR
   (Vs::Vs '["freq"])
   (freq_ (500::Float))

-- | \"Generates noise whose spectrum falls off in power by 3 dB per octave. This gives equal power over the span of each octave. This version gives 8 octaves of pink noise.\"
pinkNoise :: SDBody' a Signal
pinkNoise = addUGen $ UGen (UGName_S "PinkNoise") AR [] 1

--- randID ::
--- randID =
--- randSeed ::
--- randSeed =

-- | \"Generates noise whose spectrum has equal power at all frequencies.\"
whiteNoise :: SDBody' a Signal
whiteNoise = addUGen $ UGen (UGName_S "WhiteNoise") AR [] 1
