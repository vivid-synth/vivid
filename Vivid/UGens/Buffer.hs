{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, NoMonoLocalBinds #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Buffer (

     -- * Buffer > Info

     bufChannels
   , bufDur
   , bufFrames
   , bufRateScale
   , bufSampleRate
   , bufSamples

     -- * Buffer

   , bufRd
   , bufWr
---   , dbufrd
---   , dbufwr
---   , delTapRd
---   , delTapWr
---   , detectIndex
     -- In Vivid.UGens.InOut:
   -- , diskIn
     -- In Vivid.UGens.InOut:
   -- , diskOut
     -- In Vivid.UGens.Generators.Granular:
   -- , grainBuf
---   , harmonics
---   , index
---   , indexInBetween
---   , indexL
   , localBuf
---   , multiTap
   , phasor
   , playBuf
   , recordBuf
---   , scopeOut
---   , shaper
     -- In Vivid.UGens.Generators.Granular
   -- , tGrains
---   , tap
     -- In Vivid.UGens.InOut:
   -- , vDiskIn
     -- In Vivid.UGens.Generators.Granular
   -- , warp1
---   , wrapIndex
   ) where

import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SynthDef
import Vivid.SynthDef.FromUA
import Vivid.UGens.Args

import Data.Proxy
import GHC.Real (Ratio((:%)))

-- | Add a single LocalBuf for FFT
-- 
--   Can use 'Vivid.UGens.Args.chans_' for \"numChans\"
--   and 'frames_' for \"Vivid.UGens.Args.numFrames\"
localBuf :: Args '["numChans","numFrames"] '[] a => a -> SDBody a Signal
localBuf args = do
   mlb <- addUGen $ UGen (UGName_S "MaxLocalBufs") IR [Constant 1] 1
   numChannels' <- uaArgVal args (Proxy::Proxy "numChans")
   numFrames' <- uaArgVal args (Proxy::Proxy "numFrames")
   -- Another example where the args in sclang and scsynth are in different orders:
   addUGen $ UGen (UGName_S "LocalBuf") IR [numChannels', numFrames', mlb] 1


-- | Unlike in SC, \"doneAction\\" defaults to 2
-- 
--   Also, the default rate is the 'bufRateScale' of the buffer
playBuf :: (Args '["buf"] '["rate","trigger","startPos","loop","doneAction"] a) => a -> SDBody a Signal
playBuf args = ($ args) $ makeUGen
   "PlayBuf" AR
   (Vs::Vs '["buf","rate","trigger","startPos","loop","doneAction"])
   (rate_ defaultRate, trigger_ ((1)::Float), startPos_ ((0)::Float)
   ,loop_ ((0)::Float), doneAction_ ((2)::Float))
 where
        -- TODO: maybe don't want to do this. especially cause if you set the rate it doesn't multiply by this
        -- update docs too:
          -- todo : put bufratescale on all of em if we decide to keep this behavior
   defaultRate = bufRateScale $ uaArgVal args (V::V "buf")


-- | Unlike in SC, "doneAction" defaults to 2 and "loop" defaults to 0
recordBuf :: (Args '["buf","in"] '["offset","recLevel","preLevel","run","loop","trigger","doneAction"] a) => a -> SDBody a Signal
recordBuf = makeUGen
   "RecordBuf" AR
   (Vs::Vs '["buf","offset","recLevel","preLevel","run","loop","trigger","doneAction","in"])
   -- this is another example of different order:
   (offset_ ((0)::Float), recLevel_ ((1)::Float), preLevel_ ((0)::Float), run_ ((1)::Float), loop_ ((0)::Float), trigger_ ((1)::Float), doneAction_ ((2)::Float))

-- | Defaults to 'KR'. Can be 'IR' too but be careful that the buffer doesn't change if so!
bufChannels :: (Args '["buf"] '[] a) => a -> SDBody a Signal
bufChannels = makeUGen
   "BufChannels" KR
   (Vs::Vs '["buf"])
   NoDefaults

-- | Defaults to 'KR'. Can be 'IR' too but be careful that the buffer doesn't change if so!
bufDur :: (Args '["buf"] '[] a) => a -> SDBody a Signal
bufDur = makeUGen
   "BufDur" KR
   (Vs::Vs '["buf"])
   NoDefaults

-- bufFrames :: (Args '["buf"] '[] a) => a -> SDBody a Signal

-- | Defaults to 'KR'. Can be 'IR' too but be careful that the buffer doesn't change if so!
-- 
--   Note you don't need to use "buf_" when you use this
bufFrames :: ToSig s as => s -> SDBody' as Signal
bufFrames = (flip (.)) buf_ $ makeUGen
   "BufFrames" KR
   (Vs::Vs '["buf"])
   NoDefaults

-- bufRateScale :: (Args '["buf"] '[] a) => a -> SDBody a Signal

-- | Defaults to 'KR'. Can be 'IR' too but be careful that the buffer doesn't change if so!
-- 
--   Note you don't need to use "buf_" when you use this
bufRateScale :: ToSig s as => s -> SDBody' as Signal
bufRateScale = (. buf_) $ makeUGen
   "BufRateScale" KR
   (Vs::Vs '["buf"])
   NoDefaults

-- bufSampleRate :: (Args '["buf"] '[] a) => a -> SDBody a Signal

-- | Defaults to 'KR'. Can be 'IR' too but be careful that the buffer doesn't change if so!
-- 
--   Note you don't need to use "buf_" when you use this
bufSampleRate :: ToSig s as => s -> SDBody' as Signal
bufSampleRate = (flip (.)) buf_ $ makeUGen
   "BufSampleRate" KR
   (Vs::Vs '["buf"])
   NoDefaults

-- bufSamples :: (Args '["buf"] '[] a) => a -> SDBody a Signal

-- | Defaults to 'KR'. Can be 'IR' too but be careful that the buffer doesn't change if so!
-- 
--   Note you don't need to use "buf_" when you use this
bufSamples :: ToSig s as => s -> SDBody' as Signal
bufSamples = (flip (.)) buf_ $ makeUGen
   "BufSamples" KR
   (Vs::Vs '["buf"])
   NoDefaults

-- | \"phase\" must be at audio rate ('AR')
-- 
--   \"numChans\" can't be set after the synth is created, and must be a fixed integer
bufRd :: Args '["numChans", "buf", "phase"] '["loop", "interp"] a => a -> SDBody a [Signal]
bufRd args = do
   numChans <- uaArgVal args (V::V "numChans") >>= \case
      Constant x -> case toRational x of
         n :% 1 -> pure $ fromInteger n
         _ -> error $ "bufRd: numChans not an integer!: " ++ show x
      _ -> error "bufrd: not a fixed integer!"
   makePolyUGen numChans
      "BufRd" AR
      (Vs::Vs '["buf", "phase", "loop", "interp"])
      (loop_ ((1)::Float), interp_ ((2)::Float))
      args

-- | "phase" must be at audio rate ('AR')
bufWr :: (Args '["in", {- "numChans", -} "buf", "phase"] '["loop"] a) => a -> SDBody a Signal
bufWr = makeUGen
   "BufWr" AR
   -- An example of arguments in different orders in sclang and scsynth:
   (Vs::Vs '["buf", "phase", "loop", "in"])
   (loop_ ((1)::Float))

        -- returns demandrate
--- dbufrd ::
--- dbufrd =
--- dbufwr ::
--- dbufwr =

-- | "phase" must be the output of 'delTapWr'
--delTapRd :: (Args '["buf", "phase", "delSecs"] '["interp"] a) => s -> SDBody a Signal
--- delTapRd ::
--- delTapRd =

--- delTapWr ::
--- delTapWr =
--- detectIndex ::
--- detectIndex =
--- harmonics ::
--- harmonics =
--- index ::
--- index =
--- indexInBetween ::
--- indexInBetween =
--- indexL ::
--- indexL =
--- multiTap ::
--- multiTap =

phasor :: (Args '[] '["trigger", "rate", "start", "end", "resetPos"] a) => a -> SDBody a Signal
phasor = makeUGen
   "Phasor" AR
   (Vs::Vs '["trigger", "rate", "start", "end", "resetPos"])
   (trig_ ((0)::Float), rate_ ((1)::Float), start_ ((0)::Float), end_ ((1)::Float), resetPos_ ((0)::Float))

--- scopeOut ::
--- scopeOut =
--- shaper ::
--- shaper =
--- tap ::
--- tap =
--- wrapIndex ::
--- wrapIndex =
