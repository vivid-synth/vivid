{-# LANGUAGE DataKinds #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Triggers (
     -- in UGens.Filters:
   --   changed
---     gate
     -- in UGens.InOut
   -- , inTrig
---   , lastValue
     latch
     -- In UGens.Buffer:
   -- , phasor
   , pulseCount
   , pulseDivider
---   , sendReply
---   , sendTrig
---   , setResetFF
---   , stepper
   , sweep
     -- in UGens.Conversions:
   -- , t2a
   -- , t2k
---   , tChoose
     -- In UGens.Delays:
   -- , tDelay
---   , tExpRand
---   , tIRand
---   , tRand
---   , tWChoose
---   , tWIndex
---   , timer
---   , toggleFF
---   , trig
---   , trig1
   ) where

import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SynthDef
import Vivid.SynthDef.FromUA
import Vivid.UGens.Args

--- gate ::
--- gate =
--- lastValue ::
--- lastValue =

latch :: (Args '["in", "trigger"] '[] a) => a -> SDBody a Signal
latch = makeUGen
   "Latch" AR
   (Vs::Vs '["in", "trigger"])
   NoDefaults

pulseCount :: Args '[] '["trigger", "reset"] a => a -> SDBody a Signal
pulseCount = makeUGen
   "PulseCount" AR
   (Vs::Vs '["trigger", "reset"])
   (trig_ (0::Float), reset_ (0::Float))

-- | \"Outputs one inpulse each time it receives a certain number of triggers at its input\"
-- 
--   The trigger \"can be any signal. A trigger happens when the signal changes from
--   non-positive to positive\"
-- 
--   \"div\" is the number of pulses to divide by. Default is 2.
-- 
--   \"start\" is the starting value of the count
-- 
--   Can be 'AR' or 'KR'
pulseDivider :: Args '["trigger"] '["div", "start"] a => a -> SDBody a Signal
pulseDivider = makeUGen
   "PulseDivider" AR
   (Vs::Vs '["trigger", "div", "start"])
   (div_ (2::Float), start_ (0::Float))

--- sendReply ::
--- sendReply =
--- sendTrig ::
--- sendTrig =
--- setResetFF ::
--- setResetFF =
--- stepper ::
--- stepper =

sweep :: Args '["trigger"] '["rate"] a => a -> SDBody a Signal
sweep = makeUGen
   "Sweep" AR
   (Vs::Vs '["trigger", "rate"])
   (rate_ (1::Float))

--- tChoose ::
--- tChoose =
--- tExpRand ::
--- tExpRand =
--- tIRand ::
--- tIRand =
--- tRand ::
--- tRand =
--- tWChoose ::
--- tWChoose =
--- tWIndex ::
--- tWIndex =
--- timer ::
--- timer =
--- toggleFF ::
--- toggleFF =
--- trig ::
--- trig =
--- trig1 ::
--- trig1 =
