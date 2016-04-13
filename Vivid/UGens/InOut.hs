{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.InOut (
     -- This is deprecated in SC:
   --   audioIn
---     diskIn
---   , diskOut
---   , in__
     aIn
   , kIn
---   , inFeedback
---   , inTrig
---   , lagIn
---   , localIn
---   , localOut
---   , maxLocalBufs
---   , offsetOut
   , out
   , aOut
   , kOut
   , kOut_mono
---   , replaceOut

     -- These 2 have been deprecated in SC:
--    , sharedIn
--    , sharedOut

   , soundIn
---   , vDiskIn
---   , xOut
   ) where

import Vivid.SynthDef
import Vivid.SynthDef.FromUA
import Vivid.UGens.Algebraic
import Vivid.UGens.Args

import Data.Proxy

aIn :: (Args '["bus"] '[] a) => a -> SDBody a Signal
aIn = makeUGen
   "In" AR
   (Vs::Vs '["bus"])
   NoDefaults

--- diskIn ::
--- diskIn =
--- diskOut ::
--- diskOut =
--- in__ ::
--- in__ =
--- inFeedback ::
--- inFeedback =
--- inTrig ::
--- inTrig =

kIn :: (Args '["bus"] '[] a) => a -> SDBody a Signal
kIn = makeUGen
   "In" KR
   (Vs::Vs '["bus"])
   NoDefaults

--- lagIn ::
--- lagIn =
--- localIn ::
--- localIn =
--- localOut ::
--- localOut =
--- maxLocalBufs ::
--- maxLocalBufs =
--- offsetOut ::
--- offsetOut =

out :: (ToSig i a, ToSig busNum a) => busNum -> [i] -> SDBody' a [Signal]
out = aOut

aOut :: (ToSig i a, ToSig busNum a) => busNum -> [i] -> SDBody' a [Signal]
aOut busNum is = do
   busNum' <- toSig busNum
   is' <- mapM toSig is
   addPolyUGen $ UGen (UGName_S "Out") AR (busNum' : is') ((length::[a]->Int) is)

kOut :: (ToSig i a, ToSig busNum a) => busNum -> [i] -> SDBody' a [Signal]
kOut busNum is = do
   busNum' <- toSig busNum
   is' <- mapM toSig is
   addPolyUGen $ UGen (UGName_S "Out") KR (busNum' : is') ((length::[a]->Int) is)

-- | Temporary
kOut_mono :: (ToSig i a) => Int -> i -> SDBody' a Signal
kOut_mono busNum i = do
   i' <- toSig i
   addUGen $ UGen (UGName_S "Out") KR [Constant (toEnum busNum), i'] 1

-- kIn ::
-- kIn =

--- replaceOut ::
--- replaceOut =


-- | Audio bus input (usually mic)
soundIn :: Args '["bus"] '[] a => a -> SDBody a Signal
soundIn args = do
   bus <- args `uaArgVal` (Proxy::Proxy "bus")
   nob <- addUGen $ UGen (UGName_S "NumOutputBuses") IR [] 1 {- :: SDBody a Signal -}
   inPos <- nob ~+ bus
   addUGen $ UGen (UGName_S "In") AR [inPos] 1


--- vDiskIn ::
--- vDiskIn =

-- | "Send signal to a bus, crossfading with previous contents"
-- 
--   
--- xOut ::
--- xOut =
