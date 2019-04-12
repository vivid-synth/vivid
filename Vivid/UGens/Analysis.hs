{-# LANGUAGE DataKinds #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Analysis (

     -- * Analysis > Amplitude

     ampComp
---   , ampCompA
   , amplitude
---   , detectSilence
---   , loudness
---   , peak
---   , peakFollower
---   , sendPeakRMS

     -- * Analysis > Pitch

---   , keyTrack
   , pitch
---   , zeroCrossing

     -- * Analysis

---   , beatTrack
---   , beatTrack2
---    , mfcc
---   , onsets
     -- In UGens.Maths
   -- , runningSum
     -- In UGens.Filters.Linear:
   -- , slope
   ) where

import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SynthDef
import Vivid.SynthDef.FromUA
import Vivid.UGens.Args

-- | "Implements the (optimized) formula:
-- 
--      compensationFactor = (root / freq) ** exp
-- 
--   Higher frequencies are normally perceived as louder, which AmpComp compensates."
-- 
--   "Note that for frequencies very much smaller than root the amplitudes can become very high. In this case limit the freq with freq.max(minval), or use AmpCompA."
-- 
--   Computed at "AR", "KR", or "IR"
ampComp :: (Args '["freq", "root"] '["exponent"] a) => a -> SDBody a Signal
ampComp = makeUGen
   "AmpComp" AR
   (Vs::Vs '["freq", "root", "exponent"])
   (exponent_ (0.3333::Float))

-- | "Higher frequencies are normally perceived as louder, which AmpCompA compensates. Following the measurings by Fletcher and Munson, the ANSI standard describes a function for loudness vs. frequency.
--   Note that this curve is only valid for standardized amplitude."
-- 
--   _NOTE_ "Apart from freq, the values are not modulatable"


--- ampCompA ::
--- ampCompA =

amplitude :: Args '["in"] '["attackSecs", "releaseSecs"] a => a -> SDBody a Signal
amplitude = makeUGen
   "Amplitude" AR
   (Vs::Vs '["in", "attackSecs", "releaseSecs"])
   (attackSecs_ (0.01::Float), releaseSecs_ (0.01::Float))

--- detectSilence ::
--- detectSilence =
--- loudness ::
--- loudness =
--- peak ::
--- peak =
--- peakFollower ::
--- peakFollower =
--- sendPeakRMS ::
--- sendPeakRMS =
--- keyTrack ::
--- keyTrack =

-- | "This is a better pitch follower than ZeroCrossing, but more costly of CPU. For most purposes the default settings can be used and only in needs to be supplied."
--   
--   "[This function] returns two values [...], a freq which is the pitch estimate and hasFreq, which tells whether a pitch was found."
--
--   Note -- as this returns a 2-tuple of 'Signal's -- that you may need to be careful not
--   to accidentally use functions from the Foldable instance for (,) with the return
--   value of 'pitch'.
-- 
--   "Some vowels are still problematic, for instance a wide open mouth sound somewhere between a low pitched short 'a' sound as in 'sat', and long 'i' sound as in 'fire', contains enough overtone energy to confuse the algorithm."
-- 
--   "None of these settings are time variable."
-- 
--   Can only run at "KR"
pitch :: (Args '["in"] '["initFreq", "minFreq", "maxFreq", "execFreq", "maxBinsPerOctave", "median", "ampThreshold", "peakThreshold", "downSample", "clar"] a) => a -> SDBody a (Signal, Signal)
pitch = ((\[a,b]->(a,b)) <$>) . makePolyUGen 2
   "Pitch" KR
   (Vs::Vs '["in", "initFreq", "minFreq", "maxFreq", "execFreq", "maxBinsPerOctave", "median", "ampThreshold", "peakThreshold", "downSample", "clar"])
   (initFreq_ (440::Float), minFreq_ (60 ::Float), maxFreq_ (4000 ::Float), execFreq_ (100::Float), maxBinsPerOctave_ (16::Float), median_ (1::Float), ampThreshold_ (0.01::Float), peakThreshold_ (0.5::Float), downSample_ (1::Float), clar_ (0::Float))

--- zeroCrossing ::
--- zeroCrossing =
--- beatTrack ::
--- beatTrack =
--- beatTrack2 ::
--- beatTrack2 =
--- mfcc ::
--- mfcc =
--- onsets ::
--- onsets =
