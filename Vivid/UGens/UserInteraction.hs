{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies, NoMonoLocalBinds #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Vivid.UGens.UserInteraction (
---     keyState
---   , mouseButton
     mouseX
   , mouseY
   ) where

import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SynthDef
import Vivid.UGens.Args
import Vivid.SynthDef.FromUA

-- import Data.ByteString (ByteString)


--- keyState ::
--- keyState =
--- mouseButton ::
--- mouseButton =

-- | Only runs at 'KR'
-- 
--   Args:
--    - warp -- "Mapping curve. 0 is linear, 1 is exponential (e. g. for freq or times)"
--    - lag -- "Lag factor to dezpipper cursor movement"
mouseY :: Args '[] '["min","max","warp","lagSecs"] a => a -> SDBody a Signal
mouseY = mouseGeneral "MouseY"

-- | Only runs at 'KR'
-- 
--   Args:
--    - warp -- "Mapping curve. 0 is linear, 1 is exponential (e. g. for freq or times)"
--    - lag -- "Lag factor to dezpipper cursor movement"
mouseX :: Args '[] '["min","max","warp","lagSecs"] a => a -> SDBody a Signal
mouseX = mouseGeneral "MouseX"

mouseGeneral :: Args '[] '["min","max","warp","lagSecs"] a => String -> a -> SDBody a Signal
mouseGeneral ugenName = makeUGen
   ugenName KR
   (Vs::Vs '["min", "max", "warp", "lagSecs"])
   (min_ (0::Float), max_ (1::Float), warp_ (0::Float), lagTime_ (0.2::Float))
