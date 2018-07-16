{-# LANGUAGE DataKinds #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Generators.SingleValue (
     dc
   -- , silent
   ) where

import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SynthDef

import qualified Data.ByteString.UTF8 as UTF8 (fromString)

-- | \"This UGen simply outputs the initial value you give it\"
dc :: Float -> SDBody' a Signal
dc n =
   addUGen $ UGen (UGName_S (UTF8.fromString "DC")) AR [Constant n] 1

-- Not creating this because I don't want to clutter the namespace.
-- Just write "dc 0"!
{-
silent :: foo
silent =
-}
