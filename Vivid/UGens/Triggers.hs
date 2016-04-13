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
---   , pulseDivider
---   , sendReply
---   , sendTrig
---   , setResetFF
---   , stepper
---   , sweep
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

pulseCount :: (Args '[] '["trigger", "reset"] a) => a -> SDBody a Signal
pulseCount = makeUGen
   "PulseCount" AR
   (Vs::Vs '["trigger", "reset"])
   (trig_ (0::Float), reset_ (0::Float))

--- pulseDivider ::
--- pulseDivider =
--- sendReply ::
--- sendReply =
--- sendTrig ::
--- sendTrig =
--- setResetFF ::
--- setResetFF =
--- stepper ::
--- stepper =
--- sweep ::
--- sweep =
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
