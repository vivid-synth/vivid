{-# LANGUAGE DataKinds #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Generators.SingleValue (
     dc
   -- , silent
   ) where

import Vivid.SynthDef

import qualified Data.ByteString.Char8 as BS8 (pack)

-- | \"This UGen simply outputs the initial value you give it\"
dc :: Float -> SDBody' a Signal
dc n =
   addUGen $ UGen (UGName_S (BS8.pack "DC")) AR [Constant n] 1

-- Not creating this because I don't want to clutter the namespace.
-- Just write "dc 0"!
{-
silent :: foo
silent =
-}
