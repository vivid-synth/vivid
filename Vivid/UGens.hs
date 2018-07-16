-- | Unit Generators, which are the signal-generating/processing components of synths.
-- 
--   Most of your time reading documentation will probably be in these modules
-- 
--   **In ghci, get the type with \":i\" instead of \":t\"**
-- 
--   In \"Args '[foos] '[bars]\", \"foos\" are the required arguments, \"bars\" are the
--   optional ones (ones which have a default value provided)
-- 
--   E.g. to make a lowpass filter which filters whitenoise at 440hz, you'd write:
-- 
--   > lpf (in_ whiteNoise, freq_ 440)
-- 
--   Not all UGens from SC are here, so I've exposed the internals so you can make
--   your own. Some exports may disappear in future versions.
-- 
--   These modules are organized in the same way as the "Browse UGens" pages are
-- 
--   You can find them in:
-- 
--      - The SC IDE: In the Help Browser, click \"Browse\" -> \"UGens\"
--      - Linux: ~/.local/share/SuperCollider/Help/Browse.html#UGens
--      - Other OSes: tbd!

{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE NoUndecidableInstances #-}
{-# LANGUAGE NoIncoherentInstances #-}

{-# LANGUAGE ExistentialQuantification #-}

module Vivid.UGens (
     module Vivid.UGens.Args

   , module Vivid.UGens.Algebraic
   , module Vivid.UGens.Analysis
   , module Vivid.UGens.Buffer
   , module Vivid.UGens.Conversion
   , module Vivid.UGens.Convolution
   , module Vivid.UGens.Delays
   , module Vivid.UGens.Demand
   , module Vivid.UGens.Dynamics
   , module Vivid.UGens.Envelopes
   , module Vivid.UGens.FFT
   , module Vivid.UGens.Filters
{-
   , module Vivid.UGens.Filters.BEQSuite
   , module Vivid.UGens.Filters.Linear
   , module Vivid.UGens.Filters.Nonlinear
   , module Vivid.UGens.Filters.Pitch
-}
   , module Vivid.UGens.Info
   , module Vivid.UGens.Generators.Chaotic
   , module Vivid.UGens.Generators.Deterministic
   -- , module Vivid.UGens.Generators.Granular
   --, module Vivid.UGens.Generators.PhysicalModels
   , module Vivid.UGens.Generators.SingleValue
   , module Vivid.UGens.Generators.Stochastic
   , module Vivid.UGens.InOut
   , module Vivid.UGens.Maths
   , module Vivid.UGens.Multichannel
--   , module Vivid.UGens.Multichannel.Panners
   , module Vivid.UGens.Random
   , module Vivid.UGens.Reverbs
   , module Vivid.UGens.SynthControl
   , module Vivid.UGens.Triggers
   -- , module Vivid.UGens.Undocumented
   , module Vivid.UGens.UserInteraction
   ) where

import Vivid.UGens.Args

import Vivid.UGens.Algebraic
import Vivid.UGens.Analysis
import Vivid.UGens.Buffer
import Vivid.UGens.Conversion
import Vivid.UGens.Convolution
import Vivid.UGens.Delays
import Vivid.UGens.Demand
import Vivid.UGens.Dynamics
import Vivid.UGens.Envelopes
import Vivid.UGens.FFT
import Vivid.UGens.Filters
{-
import Vivid.UGens.Filters.BEQSuite
import Vivid.UGens.Filters.Linear
import Vivid.UGens.Filters.Nonlinear
import Vivid.UGens.Filters.Pitch
-}
import Vivid.UGens.Generators.Chaotic
import Vivid.UGens.Generators.Deterministic
-- import Vivid.UGens.Generators.Granular
--import Vivid.UGens.Generators.PhysicalModels
import Vivid.UGens.Generators.SingleValue
import Vivid.UGens.Generators.Stochastic
import Vivid.UGens.Info
import Vivid.UGens.InOut
import Vivid.UGens.Maths
import Vivid.UGens.Multichannel
--import Vivid.UGens.Multichannel.Panners
import Vivid.UGens.Random
import Vivid.UGens.Reverbs
import Vivid.UGens.SynthControl
import Vivid.UGens.Triggers
-- import Vivid.UGens.Undocumented
import Vivid.UGens.UserInteraction
