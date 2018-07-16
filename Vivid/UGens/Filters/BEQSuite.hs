-- | These UGens only run at audio rate ('AR')
-- 
--   They also can cause CPU spikes when their parameters are changed!

{-# LANGUAGE DataKinds #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Filters.BEQSuite (
     bAllpass
   , bBandPass
   , bBandStop
   , bHiPass
---   , bHiPass4
   , bHiShelf
   , bLowPass
   , bLowPass4
   , bLowShelf
   , bPeakEQ
   ) where

import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SynthDef
import Vivid.SynthDef.FromUA
import Vivid.UGens.Args

import Vivid.UGens.Algebraic
import Vivid.UGens.Filters.Linear (sos)
import Vivid.UGens.Info (sampleRate, sampleDur)

-- import qualified Data.List as L
-- import Data.Maybe
import Data.Proxy

bAllpass :: (Args '["in"] '["freq", "rq"] a) => a -> SDBody a Signal
bAllpass = makeUGen
   "BAllPass" AR
   (Vs::Vs '["in", "freq", "rq"])
   (freq_ (1200::Float), rq_ (1::Float))

-- | Band-pass filter
bBandPass :: (Args '["in"] '["freq", "bw"] a) => a -> SDBody a Signal
bBandPass = makeUGen
   "BBandPass" AR
   (Vs::Vs '["in", "freq", "bw"])
   (freq_ (1200::Float), bw_ (1::Float))

bBandStop :: (Args '["in"] '["freq", "bw"] a) => a -> SDBody a Signal
bBandStop = makeUGen
   "BBandStop" AR
   (Vs::Vs '["in", "freq", "bw"])
   (freq_ (1200::Float), bw_ (1::Float))

-- | This is only in AR
bHiPass :: (Args '["in"] '["freq", "rq"] a) => a -> SDBody a Signal
bHiPass = makeUGen
   "BHiPass" AR
   (Vs::Vs '["in", "freq", "rq"])
   (freq_ (1200::Float), rq_ (1::Float))

-- bHiPass4 :: (Args '["in"] '["freq", "rq"] a) => a -> SDBody a Signal
--- bHiPass4 ::
--- bHiPass4 =

bHiShelf :: (Args '["in"] '["freq", "rs", "db"] a) => a -> SDBody a Signal
bHiShelf = makeUGen
   "BHiShelf" AR
   (Vs::Vs '["in", "freq", "rs", "db"])
   (freq_ (1200::Float), rs_ (1::Float), db_ (0::Float))

bLowPass :: (Args '["in"] '["freq", "rq"] a) => a -> SDBody a Signal
bLowPass = makeUGen
   "BLowPass" AR
   (Vs::Vs '["in", "freq", "rq"])
   (freq_ (1200::Float), rq_ (1::Float))

-- Checked this w/ 2 different inputs:
bLowPass4 :: (Args '["in"] '["freq","rq"] a) => a -> SDBody a Signal
bLowPass4 as = do
   freq <- uaArgValWDefault (1200::Float) as (Proxy::Proxy "freq")
   rq <- uaArgValWDefault (1::Float) as (Proxy::Proxy "rq")
   in' <- as `uaArgVal` (Proxy::Proxy "in")

   -- We don't actually use 'sRate' -- why?:
   _sRate <- sampleRate
   sDur <- sampleDur
   four <- (Constant $ 2*pi)~*sDur~*freq
   five <- uOp Cos four
   six <- {- 1 ~- five -} (Constant 1) ~- five
   seven <- six ~* (Constant 0.5)
   eight <- five ~* (Constant 2)
   nine <- uOp Sin four
   ten <- nine ~* (Constant 0.5)
   eleven <- ten ~* uOp Sqrt rq
   twelve <- Constant 1 ~+ eleven
   thirteen <- uOp Recip twelve
   fourteen <- seven ~* thirteen
   fifteen <- six ~* thirteen
   sixteen <- eight ~* thirteen
   seventeen <- uOp Neg thirteen
   eighteen <- Constant 1 ~- eleven
   nineteen <- eighteen ~* seventeen
   twenty <- sos (in_ in', a0_ fourteen, a1_ fifteen, a2_ fourteen, b1_ sixteen, b2_ nineteen)
   twentyOne <- sos (in_ twenty, a0_ fourteen, a1_ fifteen, a2_ fourteen, b1_ sixteen, b2_ nineteen)
   return twentyOne

bLowShelf :: (Args '["in"] '["freq", "rs", "db"] a) => a -> SDBody a Signal
bLowShelf = makeUGen
   "BLowShelf" AR
   (Vs::Vs '["in", "freq", "rs", "db"])
   (freq_ (1200::Float), rs_ (1::Float), db_ (0::Float))


bPeakEQ :: (Args '["in"] '["freq", "rq", "db"] a) => a -> SDBody a Signal
bPeakEQ = makeUGen
   "BPeakEQ" AR
   (Vs::Vs '["in", "freq", "rq", "db"])
   (freq_ (1200::Float), rq_ (1::Float), db_ (0::Float))
