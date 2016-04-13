{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.FFT (

   -- * Pre and Post

     fft
   , ifft

     -- In Vivid.UGens.Analysis:
   -- , beatTrack
   -- , beatTrack2
     -- In Vivid.UGens.Convolution:
   -- , convolution
   -- , convolution2
   -- , convolution2L
---   , fftTrigger
---   , packFFT
     -- In Vivid.UGens.Convolution:
   -- , partConv
---   , specCentroid
---   , specFlatness
---   , specPcile
     -- In Vivid.UGens.Convolution:
   -- , stereoConvolution2L
---   , unpack1FFT
---   , unpackFFT

   -- * FFT functions

---   , pv_add
   , pv_binScramble
   , pv_binShift
---   , pv_binWipe
   , pv_brickWall
---   , pv_chainUGen
   , pv_conformalMap
   , pv_conj
---   , pv_copy
---   , pv_copyPhase
   , pv_diffuser
---   , pv_div
---   , pv_hainsworthFoote
---   , pv_jensenAndersen
   , pv_localMax
   , pv_magAbove
   , pv_magBelow
   , pv_magClip
---   , pv_magDiv
   , pv_magFreeze
---   , pv_magMul
   , pv_magNoise
   , pv_magShift
   , pv_magSmear
   , pv_magSquared
---   , pv_max
---   , pv_min
---   , pv_mul
   , pv_phaseShift
   , pv_phaseShift270
   , pv_phaseShift90
   , pv_randComb
---   , pv_randWipe
   , pv_rectComb
---   , pv_rectComb2
   ) where

import Vivid.SynthDef
import Vivid.UGens.Args
import Vivid.SynthDef.FromUA

-- | You can use "wintype_" and "winsize_" if you're used to the SC args:
fft :: (Args '["buf"] '["in", "hop", "windowType", "active", "windowSize"] a) => a -> SDBody a Signal
fft = makeUGen
   "FFT" KR
   (Vs::Vs ["buf", "in", "hop", "windowType", "active", "windowSize"])
   (in_ (0::Float), hop_ (0.5::Float), wintype_ (0::Float), active_ (1::Float), winsize_ (0::Float))

-- | You can use "wintype_" and "winsize_" if you're used to the SC args:
ifft :: (Args '["buf"] '["windowType", "windowSize"] a) => a -> SDBody a Signal
ifft =
   makeUGen "IFFT" AR
   (Vs::Vs '["buf", "windowType", "windowSize"])
   (wintype_ (0::Float), winsize_ (0::Float))

--- fftTrigger ::
--- fftTrigger =
--- packFFT ::
--- packFFT =

{-
"
Given an FFT chain, this measures the spectral centroid, which is the weighted mean frequency, or the "centre of mass" of the spectrum. (DC is ignored.)
This can be a useful indicator of the perceptual brightness of a signal.
"
-}

--- specCentroid ::
--- specCentroid =

--- specFlatness ::
--- specFlatness =

--- specPcile ::
--- specPcile =

--- unpack1FFT ::
--- unpack1FFT =
--- unpackFFT ::
--- unpackFFT =

--- pv_add ::
--- pv_add =

pv_binScramble :: (Args '["buf"] '["wipe", "width", "trigger"] a) => a -> SDBody a Signal
pv_binScramble = makeUGen
   "PV_BinScramble" KR
   (Vs::Vs '["buf", "wipe", "width", "trigger"])
   (wipe_ (0::Float), width_ (0.2::Float), trigger_ (0::Float))

pv_binShift :: (Args '["buf"] '["stretch", "shift", "interp"] a) => a -> SDBody a Signal
pv_binShift = makeUGen
   "PV_BinShift" KR
   (Vs::Vs '["buf", "stretch", "shift", "interp"])
   (stretch_ (1::Float), shift_ (0::Float), interp_ (0::Float))

--- pv_binWipe ::
--- pv_binWipe =

pv_brickWall :: (Args '["buf"] '["wipe"] a) => a -> SDBody a Signal
pv_brickWall = makeUGen
   "PV_BrickWall" KR
   (Vs::Vs '["buf", "wipe"])
   (wipe_ (0::Float))

--- pv_chainUGen ::
--- pv_chainUGen =

pv_conformalMap :: (Args '["buf"] '["aReal", "aImag"] a) => a -> SDBody a Signal
pv_conformalMap = makeUGen
   "PV_ConformalMap" KR
   (Vs::Vs '["buf", "aReal","aImag"])
   (aReal_ (0::Float), aImag_ (0::Float))

pv_conj :: (Args '["buf"] '[] a) => a -> SDBody a Signal
pv_conj = makeUGen
   "PV_Conj" KR
   (Vs::Vs '["buf"])
   NoDefaults

--- pv_copy ::
--- pv_copy =
--- pv_copyPhase ::
--- pv_copyPhase =

pv_diffuser :: (Args '["buf"] '["trigger"] a) => a -> SDBody a Signal
pv_diffuser = makeUGen
   "PV_Diffuser" KR
   (Vs::Vs '["buf", "trigger"])
   (trig_ (0::Float))

--- pv_div ::
--- pv_div =
--- pv_hainsworthFoote ::
--- pv_hainsworthFoote =
--- pv_jensenAndersen ::
--- pv_jensenAndersen =

pv_localMax :: (Args '["buf"] '["threshold"] a) => a -> SDBody a Signal
pv_localMax = makeUGen
   "PV_LocalMax" KR
   (Vs::Vs '["buf", "threshold"])
   (threshold_ (0::Float))

pv_magAbove :: (Args '["buf", "threshold"] '[] a) => a -> SDBody a Signal
pv_magAbove = makeUGen
   "PV_MagAbove" KR
   (Vs::Vs '["buf", "threshold"])
   NoDefaults

pv_magBelow :: (Args '["buf", "threshold"] '[] a) => a -> SDBody a Signal
pv_magBelow = makeUGen
   "PV_MagBelow" KR
   (Vs::Vs '["buf", "threshold"])
   NoDefaults

pv_magClip :: (Args '["buf", "threshold"] '[] a) => a -> SDBody a Signal
pv_magClip = makeUGen
   "PV_MagClip" KR
   (Vs::Vs '["buf", "threshold"])
   NoDefaults

--- pv_magDiv ::
--- pv_magDiv =

pv_magFreeze :: (Args '["buf"] '["freeze"] a) => a -> SDBody a Signal
pv_magFreeze = makeUGen
   "PV_MagFreeze" KR
   (Vs::Vs '["buf", "freeze"])
   (freeze_ (0::Float))

--- pv_magMul ::
--- pv_magMul =

pv_magNoise :: (Args '["buf"] '[] a) => a -> SDBody a Signal
pv_magNoise = makeUGen
   "PV_MagNoise" KR
   (Vs::Vs '["buf"])
   NoDefaults

pv_magShift :: (Args '["buf"] '["stretch", "shift"] a) => a -> SDBody a Signal
pv_magShift = makeUGen
   "PV_MagShift" KR
   (Vs::Vs '["buf", "stretch", "shift"])
   (stretch_ (1::Float), shift_ (0::Float))

-- | "As [the number of bins] rises, so will CPU usage."
pv_magSmear :: (Args '["buf"] '["bins"] a) => a -> SDBody a Signal
pv_magSmear = makeUGen
   "PV_MagSmear" KR
   (Vs::Vs '["buf", "bins"])
   (bins_ (0::Float))

pv_magSquared :: (Args '["buf"] '[] a) => a -> SDBody a Signal
pv_magSquared = makeUGen
   "PV_MagSquared" KR
   (Vs::Vs '["buf"])
   NoDefaults

--- pv_max ::
--- pv_max =
--- pv_min ::
--- pv_min =
--- pv_mul ::
--- pv_mul =

pv_phaseShift :: (Args '["buf", "shift"] '["integrate"] a) => a -> SDBody a Signal
pv_phaseShift = makeUGen
   "PV_PhaseShift" KR
   (Vs::Vs '["buf", "shift", "integrate"])
   (integrate_ (0::Float))

pv_phaseShift270 :: (Args '["buf"] '[] a) => a -> SDBody a Signal
pv_phaseShift270 = makeUGen
   "PV_PhaseShift270" KR
   (Vs::Vs '["buf"])
   NoDefaults

pv_phaseShift90 :: (Args '["buf"] '[] a) => a -> SDBody a Signal
pv_phaseShift90 = makeUGen
   "PV_PhaseShift90" KR
   (Vs::Vs '["buf"])
   NoDefaults

pv_randComb :: (Args '["buf"] '["wipe", "trigger"] a) => a -> SDBody a Signal
pv_randComb = makeUGen
   "PV_RandComb" KR
   (Vs::Vs '["buf", "wipe", "trigger"])
   (wipe_ (0::Float), trigger_ (0::Float))

--- pv_randWipe ::
--- pv_randWipe =

-- Possibly "numTeeth" should be required:
-- | "Alternates blocks of bins between the two inputs."
pv_rectComb :: (Args '["buf"] '["numTeeth", "phase", "width"] a) => a -> SDBody a Signal
pv_rectComb = makeUGen
   "PV_RandComb" KR
   (Vs::Vs '["buf", "numTeeth", "phase", "width"])
   (numTeeth_ (0::Float), phase_ (0::Float), width_ (0.5::Float))

-- Possibly "numTeeth" should be required:
--- pv_rectComb2 ::
--- pv_rectComb2 =
