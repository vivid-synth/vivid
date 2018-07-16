{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Filters.Pitch (
     freqShift
   , pitchShift
   , vibrato
   ) where

import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SynthDef
import Vivid.UGens.Args
import Vivid.SynthDef.FromUA

-- | \"Moves all the components of a signal by a fixed amount but does not preserve the original harmonic relationships.\" You might want 'Vivid.UGens.Filters.Pitch.pitchShift' instead.
freqShift :: (Args '["in"] '["freq", "phase"] a) => a -> SDBody a Signal
freqShift = makeUGen
   "FreqShift" AR
   (Vs::Vs '["in", "freq", "phase"])
   (freq_ (0::Float), phase_ (0::Float))

        {-
pitchShift :: In a -> Ratio a -> SDBody a Signal
pitchShift (In inp) (Ratio ratio) = do
   in' <- toSigM inp
   ratio' <- toSigM ratio
   addUGen $ UGen (UGName_S "PitchShift") AR [in', {- windowSize: -} Constant 0.2, ratio', {-pitchDispersion -} Constant 0, {- timeDispersion -} Constant 0] 1
-}

pitchShift :: (Args '["in", "ratio"] '["windowSize", "pitchDispersion", "timeDispersion"] a) => a -> SDBody a Signal
pitchShift = makeUGen
   "PitchShift" AR
   (Vs::Vs '["in", "windowSize", "ratio", "pitchDispersion", "timeDispersion"])
   (windowSize_ (0.2::Float), pitchDispersion_ (0::Float), timeDispersion_ (0::Float))

vibrato :: (Args '[] '["freq", "rate", "depth", "delaySecs", "onset", "rateVariation", "depthVariation", "iphase"] a) => a -> SDBody a Signal
vibrato = makeUGen
   "Vibrato" AR
   (Vs::Vs '["freq", "rate", "depth", "delaySecs", "onset", "rateVariation", "depthVariation", "iphase"])
   (freq_ (440::Float), rate_ (6::Float), depth_ (0.02::Float), delay_ (0::Float), onset_ (0::Float), rateVariation_ (0.04::Float), depthVariation_ (0.1::Float), iphase_ (0::Float))
