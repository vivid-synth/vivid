{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Filters.Linear (
     apf
   , bpf
   , bpz2
   , brf
   , brz2
---   , changed
   , decay
   , decay2
---   , dynKlank
   , fos
   , formlet
   , hpf
   , hpz1
   , hpz2
   , integrator
---   , klank
   , lag
   , lag2
   , lag3
   , leakDC
   , lpf
   , lpz1
   , lpz2
   , midEQ
   , onePole
   , oneZero
   , rhpf
   , rlpf
   , ramp
   , resonz
   , ringz
   , sos
   , slope
   , twoPole
   , twoZero
---   , varLag
   ) where

-- import Vivid.OSC
import Vivid.SynthDef
import Vivid.UGens.Args
import Vivid.SynthDef.FromUA

import Data.Proxy

-- import Data.ByteString (ByteString)

apf :: (Args '["in"] '["freq", "radius"] a) => a -> SDBody a Signal
apf = makeUGen
   "APF" AR
   (Vs::Vs '["in", "freq", "radius"])
   (freq_ (440::Float), radius_ (0.8::Float))

-- | Band-pass filter
-- 
--   Rq: bandwidth / cutofffreq
bpf :: (Args '["in"] '["freq", "rq"] a) => a -> SDBody a Signal
bpf = makeUGen
   "BPF" AR
   (Vs::Vs '["in", "freq", "rq"])
   (freq_ (440::Float), rq_ (1::Float))

bpz2 :: (Args '["in"] '[] a) => a -> SDBody a Signal
bpz2 = makeUGen
   "BPZ2" AR
   (Vs::Vs '["in"])
   NoDefaults

brf :: (Args '["in"] '["freq", "rq"] a) => a -> SDBody a Signal
brf = makeUGen
   "BRF" AR
   (Vs::Vs '["in", "freq", "rq"])
   (freq_ (440::Float), rq_ (1::Float))

brz2 :: (Args '["in"] '[] a) => a -> SDBody a Signal
brz2 = makeUGen
   "BRZ2" AR
   (Vs::Vs '["in"])
   NoDefaults

--- changed ::
--- changed =

decay :: (Args '["in"] '["decaySecs"] a) => a -> SDBody a Signal
decay = makeUGen
   "Decay" AR
   (Vs::Vs '["in", "decaySecs"])
   (decayTime_ (1::Float))

decay2 :: (Args '["in"] '["attackSecs", "decaySecs"] a) => a -> SDBody a Signal
decay2 = makeUGen
   "Decay2" AR
   (Vs::Vs '["in", "attackSecs", "decaySecs"])
   (attackTime_ (0.01::Float), decayTime_ (1::Float))

--- dynKlank ::
--- dynKlank =

fos :: (Args '["in"] '["a0", "a1", "b1"] a) => a -> SDBody a Signal
fos = makeUGen
   "FOS" AR
   (Vs::Vs '["in", "a0", "a1", "b1"])
   (a0_ (0::Float), a1_ (0::Float), b1_ (0::Float))

formlet :: (Args '["in"] '["freq", "attackSecs", "decaySecs"] a) => a -> SDBody a Signal
formlet = makeUGen
   "Formlet" AR
   (Vs::Vs '["in", "freq", "attackSecs", "decaySecs"])
   (freq_ (440::Float), attackTime_ (1::Float), decayTime_ (1::Float))

-- | High-pass filter
hpf :: (Args '["in"] '["freq"] a) => a -> SDBody a Signal
hpf = passFilter "HPF"

hpz1 :: (Args '["in"] '[] a) => a -> SDBody a Signal
hpz1 = makeUGen
   "HPZ1" AR
   (Vs::Vs '["in"])
   NoDefaults

hpz2 :: (Args '["in"] '[] a) => a -> SDBody a Signal
hpz2 = makeUGen
   "HPZ2" AR
   (Vs::Vs '["in"])
   NoDefaults

integrator :: (Args '["in"] '["coef"] a) => a -> SDBody a Signal
integrator = makeUGen
   "Integrator" AR
   (Vs::Vs '["in", "coef"])
   (coef_ (1::Float))

--- klank ::
--- klank =

-- | The \"lagSecs\" arg is the same as the \"lagTime\" arg in SC
--   (you can use 'Vivid.UGens.Args.lagTime_' if you like)
--
--   The calculation rate of this is whatever its \"in\" is (cool, right?)
lag :: (Args '["in"] '["lagSecs"] a) => a -> SDBody a Signal
lag as = do
   makeThing =<< getCalcRate =<< uaArgVal as (Proxy::Proxy "in")
 where
   makeThing calcRate = (flip ($)) as $ makeUGen
      "Lag" calcRate
      (Vs::Vs '["in", "lagSecs"])
      (lagTime_ (0.1::Float))

-- | 'lag2 (in_ x)' is equal to 'lag (in_ (lag (in_ x)))'
--
--   The calculation rate of this is whatever its \"in\" is
lag2 :: (Args '["in"] '["lagSecs"] a) => a -> SDBody a Signal
lag2 as =
   makeThing =<< getCalcRate =<< uaArgVal as (Proxy::Proxy "in")
 where
   makeThing calcRate = (flip ($)) as $ makeUGen
      "Lag2" calcRate
      (Vs::Vs '["in", "lagSecs"])
      (lagTime_ (0.1::Float))

-- | 'lag3 (in_ x)' is equal to 'lag (in_ $ lag (in_ $ lag (in_ x)))'
--
--   The calculation rate of this is whatever its \"in\" is
lag3 :: (Args '["in"] '["lagSecs"] a) => a -> SDBody a Signal
lag3 as =
   makeThing =<< getCalcRate =<< uaArgVal as (Proxy::Proxy "in")
 where
   makeThing calcRate = (flip ($)) as $ makeUGen
      "Lag3" calcRate
      (Vs::Vs '["in", "lagSecs"])
      (lagTime_ (0.1::Float))

-- | Note the default for both AR and KR are the same: 0.995. In SC lang, the KR
--   one defaults to 0.9.
leakDC :: (Args '["in"] '["coef"] a) => a -> SDBody a Signal
leakDC = makeUGen
   "LeakDC" AR
   (Vs::Vs '["in", "coef"])
   (coef_ (0.995::Float))

-- also look at RLPF:
-- | Low-pass filter
lpf :: (Args '["in"] '["freq"] a) => a -> SDBody a Signal
lpf = passFilter "LPF"

lpz1 :: (Args '["in"] '[] a) => a -> SDBody a Signal
lpz1 = makeUGen
   "LPZ1" AR
   (Vs::Vs '["in"])
   NoDefaults

lpz2 :: (Args '["in"] '[] a) => a -> SDBody a Signal
lpz2 = makeUGen
   "LPZ2" AR
   (Vs::Vs '["in"])
   NoDefaults

-- | 'Db' is the boost or attenuation of the signal in decibels
midEQ :: (Args '["in", "freq", "db"] '["rq"] a) => a -> SDBody a Signal
midEQ = makeUGen
   "MidEQ" AR
   (Vs::Vs '["in", "freq", "rq", "db"])
   (rq_ (1::Float))

passFilter :: (Args '["in"] '["freq"] a) => String -> a -> SDBody a Signal
passFilter filterName = makeUGen
   filterName AR
   (Vs::Vs '["in", "freq"])
   (freq_ (440::Float))

onePole :: (Args '["in"] '["coef"] a) => a -> SDBody a Signal
onePole = makeUGen
   "OnePole" AR
   (Vs::Vs '["in", "coef"])
   (coef_ (0.5::Float))

oneZero :: (Args '["in"] '["coef"] a) => a -> SDBody a Signal
oneZero = makeUGen
   "OneZero" AR
   (Vs::Vs '["in", "coef"])
   (coef_ (0.5::Float))

rhpf :: (Args '["in"] '["freq", "rq"] a) => a -> SDBody a Signal
rhpf = makeUGen
   "RHPF" AR
   (Vs::Vs '["in", "freq", "rq"])
   (freq_ (440::Float), rq_ (1::Float))

rlpf :: (Args '["in"] '["freq", "rq"] a) => a -> SDBody a Signal
rlpf = makeUGen
   "RLPF" AR
   (Vs::Vs '["in", "freq", "rq"])
   (freq_ (440::Float), rq_ (1::Float))

ramp :: (Args '["in"] '["lagSecs"] a) => a -> SDBody a Signal
ramp = makeUGen
   "Ramp" AR
   (Vs::Vs '["in", "lagSecs"])
   (lagTime_ (0.1::Float))

resonz :: (Args '["in"] '["freq", "bwr"] a) => a -> SDBody a Signal
resonz = makeUGen
   "Resonz" AR
   (Vs::Vs '["in", "freq", "bwr"])
   (freq_ (440::Float), bwr_ (1::Float))

ringz :: (Args '["in"] '["freq", "decaySecs"] a) => a -> SDBody a Signal
ringz = makeUGen
   "Ringz" AR
   (Vs::Vs '["in", "freq", "decaySecs"])
   (freq_ (440::Float), decaySecs_ (1::Float))

slope :: (Args '["in"] '[] a) => a -> SDBody a Signal
slope = makeUGen
   "Slope" AR
   (Vs::Vs '["in"])
   NoDefaults

sos :: (Args '["in"] '["a0", "a1", "a2", "b1", "b2"] a) => a -> SDBody a Signal
sos = makeUGen
   "SOS" AR
   (Vs::Vs '["in", "a0", "a1", "a2", "b1", "b2"])
   (a0_ (0::Float), a1_ (0::Float), a2_ (0::Float), b1_ (0::Float), b2_ (0::Float))

twoPole :: (Args '["in"] '["freq", "radius"] a) => a -> SDBody a Signal
twoPole = makeUGen
   "TwoPole" AR
   (Vs::Vs '["in", "freq", "radius"])
   (freq_ (440::Float), radius_ (0.8::Float))

twoZero :: (Args '["in"] '["freq", "radius"] a) => a -> SDBody a Signal
twoZero = makeUGen
   "TwoZero" AR
   (Vs::Vs '["in", "freq", "radius"])
   (freq_ (440::Float), radius_ (0.8::Float))

--- varLag ::
--- varLag =
