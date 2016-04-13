-- | **Note:** The argument format for these is a little
--   rough, and is likely to change in the future

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Envelopes (
     -- In UGens.Filters.Linear:
   -- decay
     -- In UGens.Filters.Linear:
   -- , decay2
     -- In Vivid.UGens.Demand:
   -- , demandEnvGen
     adsrGen
---   , dadsrGen
   , envGate
   , envGen
   , envGen_wGate
---   , iEnvGen
   , line
   , linen
   , percGen
   , xLine
   ) where

import Vivid.Envelopes
import Vivid.SynthDef
import Vivid.SynthDef.FromUA
-- import Vivid.SynthDef.TypesafeArgs
import Vivid.UGens.Args
import Vivid.UGens.Algebraic

import GHC.TypeLits
-- import qualified Data.Map as Map
import Data.Monoid
import Data.Proxy

-- | Defaults to 'AR'
adsrGen :: (Args '[] '["peakLevel", {- "curve", -} "bias", "gate", "doneAction"] as, ToSig attackTime (SDBodyArgs as), ToSig delayTime (SDBodyArgs as), ToSig sustainLevel (SDBodyArgs as), ToSig releaseTime (SDBodyArgs as)) => attackTime -> delayTime -> sustainLevel -> releaseTime -> EnvCurve -> as -> SDBody as Signal
adsrGen attackTime decayTime sustainLevel releaseTime curve userArgs = do
   attackTime' <- toSig attackTime
   decayTime' <- toSig decayTime
   sustainLevel' <- toSig sustainLevel
   releaseTime' <- toSig releaseTime
   peakLevel <- uaArgValWDefault (1::Float) userArgs (Proxy::Proxy "peakLevel")
   bias <- uaArgValWDefault (0::Float) userArgs (Proxy::Proxy "bias")
   doneAction <- uaArgValWDefault (0::Float) userArgs (Proxy::Proxy "doneAction")
   gate <- uaArgValWDefault (1::Float) userArgs (Proxy::Proxy "gate")

   peakXSustain <- peakLevel ~* sustainLevel'
   let plusBias :: (Signal, Signal) -> SDBody' a (Signal, Signal)
       plusBias (a, b) = do
          a' <- bias ~+ a
          return (a', b)
   biasPlusCurveSegs <- mapM plusBias [
           (peakLevel, attackTime')
         , (peakXSustain, decayTime')
         , (Constant 0, releaseTime')
         ]
-- maybe write in terms of 'dadsr' ^^
   signals <- envLiterallyToSignals $ EnvLiterally {
        _envLiterally_initialVal = bias
      , _envLiterally_releaseNode = Just 2
      , _envLiterally_offset = 0
      , _envLiterally_loopNode = Nothing
      , _envLiterally_curveSegments =
         map (\(a,b)->EnvSegment a b curve) biasPlusCurveSegs
      }
   addUGen $ UGen (UGName_S "EnvGen") AR ([
        gate
      , Constant 1 -- levelScale
      , Constant 0 -- levelBias
      , Constant 1 -- timeScale
      , doneAction -- doneActionNum doneAction
      ] <> signals) 1

--- dadsrGen ::
--- dadsrGen =


envGate :: Subset '["gate","fadeSecs"] a => SDBody' a Signal
envGate = do
   gate <- (V::V "fadeSecs") ~<= (0::Float)
   let theEnv = EnvLiterally {
            _envLiterally_initialVal = gate
          , _envLiterally_releaseNode = Just 1
          , _envLiterally_offset = 0
          , _envLiterally_loopNode = Nothing
          , _envLiterally_curveSegments = [
               EnvSegment (Constant 1) (Constant 1) Curve_Sin
             , EnvSegment (Constant 0) (Constant 1) Curve_Sin
             ]
          }

   envGen_wGate (V::V "gate") (V::V "fadeSecs") theEnv FreeEnclosing

-- | Defaults to 'AR'
envGen :: EnvLiterally a -> DoneAction -> SDBody' a Signal
envGen theEnv doneAction = do
   curveSignals <- envLiterallyToSignals theEnv
   addUGen $ UGen (UGName_S "EnvGen") AR ([
        Constant 1 -- gate
      , Constant 1 -- levelScale
      , Constant 0 -- levelBias
      , Constant 1 -- timeScale
      , Constant $ doneActionNum doneAction
      ] <> curveSignals) 1

envGen_wGate :: (ToSig gate a, ToSig timeScale a) => gate -> timeScale -> EnvLiterally a -> DoneAction -> SDBody' a Signal
envGen_wGate gate timeScale theEnv doneAction = do
   gate' <- toSig gate
   timeScale' <- toSig timeScale
   curveSignals <- envLiterallyToSignals theEnv
   addUGen $ UGen (UGName_S "EnvGen") AR ([
        gate' -- Constant 1 -- gate
      , Constant 1 -- levelScale
      , Constant 0 -- levelBias
      , timeScale' -- , Constant 1 -- timeScale
      , Constant $ doneActionNum doneAction
      ] <> curveSignals) 1


--- iEnvGen ::
--- iEnvGen =

-- | \"Generates a line from the start value to the end value.\"
-- 
--   Note this won't change after it's created, so if you'd like
--   to e.g. be able to change the \"freq\" in
-- 
--   > line (start_ 0, end_ (A::A "freq"))
-- 
--   you should write
-- 
--   > (A::A "freq") ~* line (start_ 0, end_ 1)
-- 
--   instead.
-- 
--   Defaults to KR
line :: (Args '[] '["start","end","secs","doneAction"] a) => a -> SDBody a Signal
line = makeUGen
   "Line" AR
   (Vs::Vs '["start","end","secs","doneAction"])
   (start_ (0::Float), end_ (0::Float), secs_ (1::Float), doneAction_ (0::Float))

-- | "Simple linear envelope generator"
-- 
--   Can't change after it's created -- see the note about 'line' if you want it to
-- 
--   Only computes at "KR"
linen :: (Args '[] '["gate", "attackSecs", "susLevel", "releaseSecs", "doneAction"] a) => a -> SDBody a Signal
linen = makeUGen
   "Linen" KR
   (Vs::Vs '["gate", "attackSecs", "susLevel", "releaseSecs", "doneAction"])
   (gate_ (1::Float), attackTime_ (0.01::Float), susLevel_ (1::Float), releaseTime_ (1::Float), doneAction_ (0::Float))


-- | Percussive hit
-- 
--   'doneAction' is currently 2 but may either be 0 or 2 in future versions
percGen :: (Args '[] '["attackSecs", "releaseSecs", "level", "curve", "doneAction"] a) => a -> SDBody a Signal
percGen userArgs = do
   level <- uaArgWDef_onlyConst (1::Float) userArgs (V::V "level")
   attackTime <- uaArgWDef_onlyConst (0.01::Float) userArgs (V::V "attackSecs")
   releaseTime <- uaArgWDef_onlyConst (1::Float) userArgs (V::V "releaseSecs")
   curve <- uaArgWDef_onlyConst (-4::Float) userArgs (V::V "curve")
   doneAction <- fromEnum <$> uaArgWDef_onlyConst (2::Float) userArgs (V::V "doneAction")

   envGen (env 0 [(level, attackTime), (0, releaseTime)] (Curve_Curve curve)) (DoneAction_AsNum doneAction)
 where
   uaArgWDef_onlyConst defaultVal args argName =
      uaArgValWDefault defaultVal args argName >>= \case
         Constant x -> return x
         _ -> error $ "bad argument type: "<>show (symbolVal argName)<>" wasn't a Constant"

-- | \"Generates an exponential curve from the start value to the end value. Both the start and end values must be non-zero and have the same sign.\"
-- 
-- Defaults to KR
xLine :: (Args '[] '["start","end","secs","doneAction"] a) => a -> SDBody a Signal
xLine = makeUGen
   "XLine" KR
   (Vs::Vs '["start","end","secs","doneAction"])
   (start_ (1::Float), end_ (2::Float), secs_ (1::Float), doneAction_ (0::Float))
