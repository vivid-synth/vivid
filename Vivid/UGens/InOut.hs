{-# LANGUAGE
     DataKinds
   , ExtendedDefaultRules
   , LambdaCase
   , OverloadedStrings

   , NoIncoherentInstances
   , NoMonomorphismRestriction
   , NoUndecidableInstances
   #-}

module Vivid.UGens.InOut (
     -- This is deprecated in SC:
   --   audioIn
---     diskIn
     diskOut
---   , in__
   , aIn
   , kIn
---   , inFeedback
---   , inTrig
---   , lagIn
   , localIn
   , localOut
---   , maxLocalBufs
---   , offsetOut
   , out
   , out'
   , aOut
--- , kOut
--- , kOut_mono
   , replaceOut

     -- These 2 have been deprecated in SC:
--    , sharedIn
--    , sharedOut

   , soundIn
---   , vDiskIn
---   , xOut
   ) where

import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SC.Server.Types (BufferId(..))
import Vivid.SynthDef
import Vivid.SynthDef.FromUA
import Vivid.UGens.Algebraic
import Vivid.UGens.Args

import Data.Proxy

aIn :: Args '["bus"] '[] a => a -> SDBody a Signal
aIn = makeUGen
   "In" AR
   (Vs::Vs '["bus"])
   NoDefaults

--- diskIn ::
--- diskIn =

-- | 'buf' is a temporary buffer to accumulate before writing.
-- 
--    "NOTE: The Buffer's numFrames must be a power of two and is recommended to be at least 65536 -- preferably 131072 or 262144. Smaller buffer sizes mean more frequent disk access, which can cause glitches."
--
--    65536 == 2 ^ 16
--    131072 == 2 ^ 17
--    262144 == 2 ^ 18
-- 
--    For ease of use with 'sd' this has output type \"[Signal]\", but the list
--      is always empty
diskOut :: ToSig s a => BufferId -> [s] -> SDBody' a [Signal]
diskOut (BufferId bufId) sigs = do
   sigs' <- mapM toSig sigs
   addPolyUGen $ UGen (UGName_S "DiskOut") AR (Constant (realToFrac bufId) : sigs') 0

--- in__ ::
--- in__ =
--- inFeedback ::
--- inFeedback =
--- inTrig ::
--- inTrig =

kIn :: Args '["bus"] '[] a => a -> SDBody a Signal
kIn = makeUGen
   "In" KR
   (Vs::Vs '["bus"])
   NoDefaults

--- lagIn ::
--- lagIn =

-- localIn :: Args '[] '["default"] a => Int -> a -> SDBody a [Signal]
-- "default" is 0 for now:
localIn :: Int -> SDBody' a [Signal]
localIn numChans = do
   addPolyUGen $ UGen (UGName_S "LocalIn") AR [Constant 0] numChans

localOut :: ToSig s as => [s] -> SDBody' as ()
localOut inSig = do
   sigs <- mapM toSig inSig
   addPolyUGen (UGen (UGName_S "LocalOut") AR sigs 0) >>= \case
      [] -> pure ()
      _ -> error "??? (23s0g)"
   pure ()


--- maxLocalBufs ::
--- maxLocalBufs =
--- offsetOut ::
--- offsetOut =

out :: (ToSig i a, ToSig busNum a) => busNum -> [i] -> SDBody' a [Signal]
out = aOut

out' :: (Elem "out" a, ToSig i a) => [i] -> SDBody' a [Signal]
out' = out (V::V "out")

aOut :: (ToSig i a, ToSig busNum a) => busNum -> [i] -> SDBody' a [Signal]
aOut busNum is = do
   busNum' <- toSig busNum
   is' <- mapM toSig is
   addPolyUGen $ UGen (UGName_S "Out") AR (busNum' : is') ((length::[a]->Int) is)

-- kOut

-- kIn ::
-- kIn =

-- todo: does this work/is it robust?:
replaceOut :: (ToSig i a, ToSig busNum a) => busNum -> [i] -> SDBody' a [Signal]
replaceOut busNum is = do
   busNum' <- toSig busNum
   is' <- mapM toSig is
   addPolyUGen $ UGen (UGName_S "ReplaceOut") AR (busNum' : is') ((length::[a]->Int) is)

-- | Audio bus input (usually mic)
soundIn :: Args '["bus"] '[] a => a -> SDBody a Signal
soundIn args = do
   bus <- args `uaArgVal` (Proxy::Proxy "bus")
   nob <- addUGen $ UGen (UGName_S "NumOutputBuses") IR [] 1 {- :: SDBody a Signal -}
   inPos <- nob ~+ (bus :: Signal)
   addUGen $ UGen (UGName_S "In") AR [inPos] 1


--- vDiskIn ::
--- vDiskIn =

-- | "Send signal to a bus, crossfading with previous contents"
-- 
--   
--- xOut ::
--- xOut =
