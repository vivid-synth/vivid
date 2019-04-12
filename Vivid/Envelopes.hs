-- | Envelopes. **This module is the least mature in vivid and is likely to
--   change a lot in the future!**

{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE
     DataKinds
   , ExistentialQuantification
   , KindSignatures
   , LambdaCase

   , NoIncoherentInstances
   , NoMonomorphismRestriction
   , NoUndecidableInstances

   , FlexibleContexts
   #-}

module Vivid.Envelopes (
     EnvLiterally(..)
   , envLiterallyToSignals
   , env

   , EnvCurve(..)
   , EnvSegment(..)
   , shapeNumber
   , curveNumber
   ) where

import Vivid.SynthDef.ToSig (ToSig(..)) -- , SDBody)
import Vivid.SynthDef.Types
-- import Vivid.SynthDef.FromUA (fromUA, Args)

-- import Data.Int
import Data.Monoid
import GHC.TypeLits
import Prelude -- BBP

data EnvLiterally (args :: [Symbol])
   = forall initial. (ToSig initial args) =>
     EnvLiterally {
    _envLiterally_initialVal :: initial
   -- ,_envLiterally_numSegments :: Int -- i know this is a 'Literally' but it's so computable...
  , _envLiterally_releaseNode :: Maybe Int
  , _envLiterally_offset :: Float -- ?? -- only used for 'IEnvGen', which i dont have
    -- invariant: releasenode must be larger (or equal?) to/than loopnode
    -- also, if one is Just, i think the other must be -- so if that's true, use
    -- a different data structure
    -- at least for the non-'Literally' one
  , _envLiterally_loopNode :: Maybe Int
  , _envLiterally_curveSegments :: [EnvSegment]
  }
 -- deriving (Show, Eq)

{-
  InputSpec_UGen {_inputSpec_uGen_index = 2, _inputSpec_uGen_outputIndex = 0}
  Constant: 2.0 (index 2) -- length segments
  Constant: 1.0 (index 1) -- release node
  Constant: -99.0 (index 3) -- loop node

signals?:
  Constant: 1.0 (index 1)
  Constant: 1.0 (index 1)
  Constant: 3.0 (index 4)
  Constant: 0.0 (index 0)
  Constant: 0.0 (index 0)
  Constant: 1.0 (index 1)
  Constant: 3.0 (index 4)
  Constant: 0.0 (index 0)
-}

-- i think this is (only) for the arguments to EnvGen:
envLiterallyToSignals :: EnvLiterally (b::[Symbol]) -> SDBody' b [Signal]
envLiterallyToSignals envLiterally@(EnvLiterally a _ _ _ _) = do
   {-
  foo <- case _envLiterally_initialVal envLiterally of
    x -> toSigM $ x
-}
  foo <- toSig a
  return $ [
     foo
   , Constant . toEnum . (length::[a]->Int) $ _envLiterally_curveSegments envLiterally
   , Constant $ case _envLiterally_releaseNode envLiterally of
        Just x -> toEnum x
        Nothing -> (-99)
   , Constant $ case _envLiterally_loopNode envLiterally of
        Just x -> toEnum x
        Nothing -> (-99)
   ] <> concatMap envSegmentToSignals (_envLiterally_curveSegments envLiterally)
 where
   envSegmentToSignals :: EnvSegment -> [Signal]
   envSegmentToSignals envSegment = [
        _envSegment_targetVal envSegment
      , _envSegment_timeToGetThere envSegment
      , Constant $ envCurveNumber $ _envSegment_curve envSegment
      , Constant $ envCurveFloatNumber $ _envSegment_curve envSegment
      ]

data EnvSegment
   = EnvSegment {
    _envSegment_targetVal :: Signal
  , _envSegment_timeToGetThere :: Signal
  , _envSegment_curve :: EnvCurve
  }
 deriving (Show, Eq)

data EnvCurve
   = Curve_Step
   | Curve_Linear
   | Curve_Lin
   | Curve_Exponential
   | Curve_Exp
   | Curve_Sine
   | Curve_Sin
   | Curve_Welch
   | Curve_Wel
   | Curve_Squared
   | Curve_Sqr
   | Curve_Cubed
   | Curve_Cub
   | Curve_Curve Float -- ^ 0 is linear, positive curves up, negative curves down
 deriving (Show, Eq)

-- | Same as \"Env.shapeNumber\" in SC.
-- 
--   This is useful if you want to set a the env shape of a running synth
shapeNumber :: EnvCurve -> Float
shapeNumber = envCurveNumber

-- | "shapeNumber" with a name I like better
curveNumber :: EnvCurve -> Float
curveNumber = shapeNumber

envCurveNumber :: EnvCurve -> Float
envCurveNumber = \case
   Curve_Step -> 0

   Curve_Linear -> 1
   Curve_Lin -> 1

   Curve_Exponential -> 2
   Curve_Exp -> 2

   Curve_Sine -> 3
   Curve_Sin -> 3

   Curve_Welch -> 4
   Curve_Wel -> 4

   Curve_Squared -> 6
   Curve_Sqr -> 6

   Curve_Cubed -> 7
   Curve_Cub -> 7

   Curve_Curve _ -> 5

envCurveFloatNumber :: EnvCurve -> Float
envCurveFloatNumber = \case
   Curve_Curve f -> f
   _ -> 0


env :: Float -> [(Float, Float)] -> EnvCurve -> EnvLiterally a
env firstPoint pointsAndLengths curve = EnvLiterally {
     _envLiterally_initialVal = firstPoint
   , _envLiterally_releaseNode = Nothing
   , _envLiterally_offset = 0
   , _envLiterally_loopNode = Nothing
   , _envLiterally_curveSegments = map foo pointsAndLengths
   }
 where
   foo :: (Float, Float) -> EnvSegment
   foo (point, dur) = EnvSegment {
        _envSegment_targetVal = Constant point
      , _envSegment_timeToGetThere = Constant dur
         -- this is, of course, limiting:
      , _envSegment_curve = curve
      }
