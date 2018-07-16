{-# LANGUAGE DataKinds #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.SynthControl (
   -- Might not actually need these 2 directly:
---     control
---   , controlName

     -- In Vivid.UGens.Analysis:
   -- , detectSilence
---   , done
---   , free__
     freeSelf
---   , freeSelfWhenDone
---   , lagControl
---   , namedControl
---   , pause
---   , pauseSelf
---   , pauseSelfWhenDone
---   , trigControl
   ) where

import Vivid.UGens.Args
import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SynthDef
import Vivid.SynthDef.FromUA

   -- Might not actually need these 2 directly:
--- control ::
--- control =
--- controlName ::
--- controlName =

--- done ::
--- done =
--- free__ ::
--- free__ =

-- | Frees the synth when the trigger changes from non-positive to positive
-- 
--   Runs at 'KR'
freeSelf :: Args '["trigger"] '[] a => a -> SDBody a Signal
freeSelf = makeUGen
   "FreeSelf" KR
   (Vs::Vs '["trigger"])
   NoDefaults

--- freeSelfWhenDone ::
--- freeSelfWhenDone =

--- lagControl ::
--- lagControl =
--- namedControl ::
--- namedControl =
--- pause ::
--- pause =
--- pauseSelf ::
--- pauseSelf =
--- pauseSelfWhenDone ::
--- pauseSelfWhenDone =
--- trigControl ::
--- trigControl =
