{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, NoMonoLocalBinds #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Dynamics (
     compander
---   , companderD
   , limiter
   , normalizer
   ) where

import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SynthDef
import Vivid.SynthDef.FromUA
--- import Vivid.SynthDef.TypesafeArgs
import Vivid.UGens.Args

compander :: (Args '["in"] '["control","threshold","slopeBelow","slopeAbove","clampSecs","relaxSecs"] a) => a -> SDBody a Signal
compander = makeUGen
   "Compander" AR
   (Vs::Vs '["in","control","threshold","slopeBelow","slopeAbove","clampSecs","relaxSecs"])
   (control_ (0::Float), thresh_ (0.5::Float), slopeBelow_ (1::Float), slopeAbove_ (1::Float), clampTime_ (0.01::Float), relaxTime_ (0.1::Float))


--- companderD ::
--- companderD =

-- | Note this can only run at "AR"
-- 
--   \"secs\" is the lookahead time -- if you're coming from SC you can use 'Vivid.UGens.Args.dur_' for consistency
limiter :: (Args '["in"] '["level", "secs"] a) => a -> SDBody a Signal
limiter = makeUGen
   "Limiter" AR
   (Vs::Vs '["in","level","secs"])
   (level_ (1::Float), secs_ (0.01::Float))

-- | Note this can only run at "AR"
-- 
--   \"secs\" is the lookahead time -- if you're coming from SC you can use 'Vivid.UGens.Args.dur_'
normalizer :: (Args '["in"] '["level", "secs"] a) => a -> SDBody a Signal
normalizer = makeUGen
   "Normalizer" AR
   (Vs::Vs '["in","level","secs"])
   (level_ (1::Float), secs_ (0.01::Float))
