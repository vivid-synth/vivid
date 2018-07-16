{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Delays (

     -- * Delays > Buffer

---     bufAllpassC
---   , bufAllpassL
---   , bufAllpassN
---   , bufCombC
---   , bufCombL
---   , bufCombN
---   , bufDelayC
---   , bufDelayL
---   , bufDelayN
     -- in Vivid.UGens.Buffer:
   -- , multiTap
---   , pingPong
     -- in Vivid.UGens.Buffer:
   -- , tap

     -- * Delays

     allpassC
   , allpassL
   , allpassN
   , combC
   , combL
   , combN
     -- in Vivid.UGens.Buffer:
   -- , delTapRd
   -- , delTapWr
   , delay1
   , delay2
   , delayC
   , delayL
   , delayN
   , pluck
---   , tDelay
   ) where

-- import Data.ByteString (ByteString)

import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SynthDef
import Vivid.SynthDef.FromUA
import Vivid.UGens.Args

--- bufAllpassC ::
--- bufAllpassC =
--- bufAllpassL ::
--- bufAllpassL =
--- bufAllpassN ::
--- bufAllpassN =
--- bufCombC ::
--- bufCombC =
--- bufCombL ::
--- bufCombL =
--- bufCombN ::
--- bufCombN =
--- bufDelayC ::
--- bufDelayC =
--- bufDelayL ::
--- bufDelayL =
--- bufDelayN ::
--- bufDelayN =
--- pingPong ::
--- pingPong =


allpassC :: (Args '["in"] ["maxDelaySecs", "delaySecs", "decaySecs"] a) => a -> SDBody a Signal
allpassC = makeDelay "AllpassC"

allpassL :: (Args '["in"] ["maxDelaySecs", "delaySecs", "decaySecs"] a) => a -> SDBody a Signal
allpassL = makeDelay "AllpassL"

allpassN :: (Args '["in"] ["maxDelaySecs", "delaySecs", "decaySecs"] a) => a -> SDBody a Signal
allpassN = makeDelay "AllpassN"

-- These 3 have a delay -- the above 3 you hear yourself right away:

combC :: (Args '["in"] ["maxDelaySecs", "delaySecs", "decaySecs"] a) => a -> SDBody a Signal
combC = makeDelay "CombC"

combL :: (Args '["in"] ["maxDelaySecs", "delaySecs", "decaySecs"] a) => a -> SDBody a Signal
combL = makeDelay "CombL"

combN :: (Args '["in"] ["maxDelaySecs", "delaySecs", "decaySecs"] a) => a -> SDBody a Signal
combN = makeDelay "CombN"

makeDelay :: String -> (Args '["in"] ["maxDelaySecs", "delaySecs", "decaySecs"] a) => a -> SDBody a Signal
makeDelay delayName =
   makeUGen delayName AR
   (Vs::Vs '["in", "maxDelaySecs", "delaySecs", "decaySecs"])
   (maxDelayTime_ (0.2::Float), delayTime_ (0.2::Float), decayTime_ ((1)::Float))

delay1 :: (Args '["in"] '[] a) => a -> SDBody a Signal
delay1 = makeUGen
   "Delay1" AR
   (Vs::Vs '["in"])
   NoDefaults

delay2 :: (Args '["in"] '[] a) => a -> SDBody a Signal
delay2 = makeUGen
   "Delay2" AR
   (Vs::Vs '["in"])
   NoDefaults

delayC :: (Args '["in"] '["maxDelaySecs", "delaySecs"] a) => a -> SDBody a Signal
delayC = makeUGen
   "DelayC" AR
   (Vs::Vs '["in", "maxDelaySecs", "delaySecs"])
   (maxDelayTime_ (0.2::Float), delayTime_ (0.2::Float))

delayL :: (Args '["in"] '["maxDelaySecs", "delaySecs"] a) => a -> SDBody a Signal
delayL =  makeUGen
   "DelayL" AR
   (Vs::Vs '["in", "maxDelaySecs", "delaySecs"])
   (maxDelayTime_ (0.2::Float), delayTime_ (0.2::Float))

delayN :: (Args '["in"] '["maxDelaySecs", "delaySecs"] a) => a -> SDBody a Signal
delayN =  makeUGen
   "DelayN" AR
   (Vs::Vs '["in", "maxDelaySecs", "delaySecs"])
   (maxDelayTime_ (0.2::Float), delayTime_ (0.2::Float))

pluck :: Args '[] '["in","trigger","maxDelaySecs","delaySecs","decaySecs","coef"] a => a -> SDBody a Signal
pluck = makeUGen
   "Pluck" AR
   (Vs::Vs '["in","trigger","maxDelaySecs","delaySecs","decaySecs","coef"])
   (in_ (0::Float), trigger_ (1::Float), maxDelaySecs_ (0.2::Float)
   ,delaySecs_ (0.2::Float), decaySecs_ (1::Float), coef_ (0.5::Float))

--- tDelay ::
--- tDelay =
