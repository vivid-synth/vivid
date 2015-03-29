{-# OPTIONS_HADDOCK show-extensions #-}

-- | For an intro to all this, check out <http://amindfv.com/vivid> or the "Vivid.SynthDef" module

module Vivid (
     sleep
   , module Vivid.SynthDef
   , module Vivid.UGens
   , module Vivid.SCServer
   ) where

import Vivid.SCServer
import Vivid.SynthDef
import Vivid.UGens

import Control.Concurrent (threadDelay)

sleep :: Float -> IO ()
sleep t = threadDelay . fromEnum $ t * 1e6
