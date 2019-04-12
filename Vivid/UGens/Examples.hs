{-# LANGUAGE DataKinds #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Vivid.UGens.Examples where

import Vivid

import Vivid.SC.SynthDef.Types

-- | 'Dbrown' example from the SC help file
dbrown_example :: SDBody' args [Signal]
dbrown_example = do
   mx <- mouseX (min_ 1, max_ 40, warp_ 1)
   imp <- impulse (freq_ mx) ? KR
   dbr <- dbrown (lo_ 0, hi_ 15, step_ 1, length_ inf)
   dem <- demand (trig_ imp, reset_ 0, ugen_ dbr)
   s <- 0.1 ~* sinOsc (freq_ $ dem ~* 30 ~+ 340)
   out 0 [s,s]
