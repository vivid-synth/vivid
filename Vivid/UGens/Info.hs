{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Info (
     checkBadValues
   , controlDur
   , controlRate
     -- In Vivid.UGens.Demand:
   -- , dpoll
   , numAudioBuses
   , numBuffers
   , numControlBuses
   , numInputBuses
   , numOutputBuses
   , numRunningSynths
   , poll
   , radiansPerSample
   , sampleDur
   , sampleRate
   , subsampleOffset
   ) where

import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SynthDef
import Vivid.SynthDef.FromUA
import Vivid.UGens.Args

checkBadValues :: (Args '["in"] '["id", "post"] a) => a -> SDBody a Signal
checkBadValues = makeUGen
   "CheckBadValues" AR
   (Vs::Vs '["in", "id", "post"])
   (id_ (0::Float), post_ (2::Float))

-- | The (current) duration of a control block on the server in seconds
-- 
--   Equal to @1 ~/ controlRate@
controlDur :: SDBody' a Signal
controlDur =
   addUGen $ UGen (UGName_S "ControlDur") IR [] 1

-- | The current control rate of the server
-- 
--   Equal to @1 ~/ controlDur@
controlRate :: SDBody' a Signal
controlRate =
   addUGen $ UGen (UGName_S "ControlRate") IR [] 1

-- | The number of audio buses
numAudioBuses :: SDBody' a Signal
numAudioBuses =
   addUGen $ UGen (UGName_S "NumAudioBuses") IR [] 1

-- | The number of open buffers
numBuffers :: SDBody' a Signal
numBuffers =
   addUGen $ UGen (UGName_S "NumBuffers") IR [] 1

numControlBuses :: SDBody' a Signal
numControlBuses =
   addUGen $ UGen (UGName_S "NumControlBuses") IR [] 1

numInputBuses :: SDBody' a Signal
numInputBuses =
   addUGen $ UGen (UGName_S "NumInputBuses") IR [] 1

numOutputBuses :: SDBody' a Signal
numOutputBuses =
   addUGen $ UGen (UGName_S "NumOutputBuses") IR [] 1

numRunningSynths :: SDBody' a Signal
numRunningSynths =
   addUGen $ UGen (UGName_S "NumRunningSynths") IR [] 1

-- } This is CPU-intensive: only use for debugging!
-- 
--   First argument is frequency of polling: the number of times per second to poll
-- 
--   Returns the UGen it's polling so you can add \"poll\" to an existing
--   signal chain without altering it
poll :: Float -> String -> SDBody' a Signal -> SDBody' a Signal
poll freq label polledThing = do
   imp <- addUGen $ UGen (UGName_S "Impulse") AR [Constant freq] 1
   pt <- polledThing
   let args = ([imp, pt] ++) $ map (Constant . realToFrac) $
        [-1, (length::[a]->Int) label] ++ map fromEnum label
   _ <- addUGen $ UGen (UGName_S "Poll") AR args 1
   pure pt

radiansPerSample :: SDBody' a Signal
radiansPerSample =
   addUGen $ UGen (UGName_S "RadiansPerSample") IR [] 1

sampleDur :: SDBody' a Signal
sampleDur =
   addUGen $ UGen (UGName_S "SampleDur") IR [] 1

sampleRate :: SDBody' a Signal
sampleRate =
   addUGen $ UGen (UGName_S "SampleRate") IR [] 1

subsampleOffset :: SDBody' a Signal
subsampleOffset =
   addUGen $ UGen (UGName_S "SubsampleOffset") IR [] 1
