-- | Most of these only run at audio rate ('AR')

{-# LANGUAGE DataKinds #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Convolution (
     convolution
   , convolution2
   , convolution2L
   , convolution3
   , partConv
---   , stereoConvolution2L
   ) where

import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.UGens.Args
import Vivid.SynthDef
import Vivid.SynthDef.FromUA

convolution :: (Args '["in", "kernel"] '["frameSize"] a) => a -> SDBody a Signal
convolution = makeUGen
   "Convolution" AR
   (Vs::Vs '["in", "kernel", "frameSize"])
   (frameSize_ ((512)::Float))

convolution2 :: (Args '["in", "kernel"] '["trigger", "frameSize"] a) => a -> SDBody a Signal
convolution2 = makeUGen
   "Convolution2" AR
   (Vs::Vs '["in", "kernel", "trigger", "frameSize"])
   (trigger_ ((0)::Float), frameSize_ ((2048)::Float))

convolution2L :: (Args '["in", "kernel"] '["trigger", "frameSize", "crossFade"] a) => a -> SDBody a Signal
convolution2L = makeUGen
   "Convolution2L" AR
   (Vs::Vs '["in", "kernel", "trigger", "frameSize", "crossFade"])
   (trigger_ ((0)::Float), frameSize_ ((2048)::Float), crossFade_ (((1)::Float)))

-- | This one can run at control rate ('KR').
--   It's inefficient so only useful for very small kernels or control rate.
convolution3 :: (Args '["in", "kernel"] '["trigger", "frameSize"] a) => a -> SDBody a Signal
convolution3 = makeUGen
   "Convolution3" AR
   (Vs::Vs '["in", "kernel", "trigger", "frameSize"])
   (trigger_ ((0)::Float), frameSize_ ((2048)::Float))

partConv :: (Args '["in", "fftSize", "irBufNum"] '[] a) => a -> SDBody a Signal
partConv = makeUGen
   "PartConv" AR
   (Vs::Vs '["in", "fftSize", "ifBufNum"])
   NoDefaults

--- stereoConvolution2L ::
--- stereoConvolution2L =
