-- | Synth Definitions in SuperCollider are how you define the way synths should sound
--   -- you describe parameters and a graph of sound generators, add them to the server
--   with 'defineSD', and then create instances of the Synth Definition (called "synths"),
--   which each play separately. You can set parameters of the synth at any time while
--   they're playing
-- 
--   Usually, you shouldn't be making 'SynthDef's explicitly -- there's a state monad
--   'SDBody' which lets you construct synthdefs like so:
-- 
--   @
--   test :: SynthDef
--   test = 'sd' (0 ::I \"note\") $ do
--      s <- 0.1 'Vivid.UGens.~*' 'Vivid.UGens.sinOsc' (freq_ $ 'Vivid.UGens.midiCPS' (V::V \"note\"))
--      out 0 [s, s]
--   @
-- 
--   You then optionally explicitly send the synth definition to the SC server with
-- 
--   >>> defineSD test
-- 
--   You then create a synth from the synthdef like:
-- 
--   >>> s <- synth test (45 ::I "note")
-- 
--   This returns a 'NodeId' which is a reference to the synth, which you can
--   use to e.g. change the params of the running synth with e.g.
-- 
--   >>> set s (38 ::I "note")
-- 
--   Then you can free it (stop its playing) with
-- 
--   >>> free s
-- 
--   (If you want interop with SClang, use "sdNamed" and "synthNamed")

{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE NoRebindableSyntax #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.SynthDef (
  -- * Synth actions

  -- * Synth Definition Construction
    SynthDef(..)
  , UGen(..)
  , addUGen
  , addMonoUGen
  , addPolyUGen
  , ToSig(..)
  , Signal(..)
  , encodeSD
--  , defineSD
  , sd
  , sdNamed
  , sdPretty
  , (?)
--  , play
--  , cmdPeriod
  , DoneAction(..)
  , doneActionNum
  , sdLitPretty
  , sdToLiteral
  -- literalToSD

  , execState

  , getCalcRate

  -- * Built-in Unit Generator Operations

  , UnaryOp(..)
  , uOpToSpecialI
  , specialIToUOp

  , BinaryOp(..)
  , biOpToSpecialI
  , specialIToBiOp

  , module Vivid.SynthDef.Types

  , getSDHashName

  , makeSynthDef

  , shrinkSDArgs

  , SDBody
  ) where

import Vivid.SynthDef.ToSig
import Vivid.SynthDef.Literally as Literal
import Vivid.SynthDef.Types
import Vivid.SynthDef.FromUA (SDBody)

-- import Control.Applicative
import Control.Arrow (first{-, second-})
import Control.Monad.State (get, put, modify, execState)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8 (pack)
import Data.Hashable (Hashable, hashWithSalt, hash)
import Data.Int
import Data.List (nub, elemIndex, find) -- , sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
-- import qualified Data.Set as Set
import Prelude

sdPretty :: SynthDef a -> String
sdPretty synthDef = unlines $ [
     "Name: " <> show (_sdName synthDef)
   , "Args: " <> show (_sdParams synthDef)
   , "UGens: "
   ] <> map show (Map.toAscList (_sdUGens synthDef))

-- | Action to take with a UGen when it's finished
-- 
--   This representation will change in the future
data DoneAction
   = DoNothing
   | FreeEnclosing
   | DoneAction_AsNum Int
 deriving (Show, Eq)

doneActionNum :: DoneAction -> Float
doneActionNum = \case
   DoNothing -> 0
   FreeEnclosing -> 2
   DoneAction_AsNum n -> toEnum n

--invariants (to check):
-- param names don't clash
-- graph is real and acyclic
-- no "dangling" pieces -- sign that something's wrong
-- params are all used, and the ones that're used in the graph all exist


sdToLiteral :: SynthDef a -> Literal.LiteralSynthDef
sdToLiteral theSD@(SynthDef name params ugens) = fixAndSimplify $
   LiteralSynthDef
      (case name of
         SDName_Named s -> s
         SDName_Hash -> getSDHashName theSD
         )
      (gatherConstants $ Map.toAscList ugens )
      (map snd params)
      (zipWith (\s i -> ParamName s i) (map fst params) [0..])
      (makeUGenSpecs params $ Map.toAscList ugens)
      []

fixAndSimplify :: Literal.LiteralSynthDef -> Literal.LiteralSynthDef
fixAndSimplify =
   replaceBitNot


-- Can unit test this by making a complex SD graph that uses
-- multiple "bitNot"s and checking that it's exactly equal to if
-- we'd used '& . bitXor 0xFFFFFF'

-- silent:
-- play $ (0.1 ~* sinOsc (freq_ 440)) >>= \x -> uOp BitNot x >>= \y -> biOp BitAnd y x >>= \z -> out 0 [z,z]

-- | Fix for github.com/supercollider/supercollider/issues/1749
replaceBitNot :: Literal.LiteralSynthDef -> Literal.LiteralSynthDef
replaceBitNot lsd@(Literal.LiteralSynthDef name oldConsts params paramNames ugens variants) =
   case any isBitNot ugens of
      False -> lsd
      True ->
         Literal.LiteralSynthDef name newConsts params paramNames (map replaceIt ugens) variants
 where
   -- newConsts :: [Float];  newOneLoc :: Int32
   (newConsts, toEnum -> negOneLoc) =
      case elemIndex (-1) oldConsts of
         Nothing -> (oldConsts <> [(-1)], (length::[a]->Int) oldConsts)
         Just i -> (oldConsts, i)
   isBitNot :: UGenSpec -> Bool
   isBitNot ug =
         (Literal._uGenSpec_name ug == "UnaryOpUGen")
      && (Literal._uGenSpec_specialIndex ug == uOpToSpecialI BitNot)
   replaceIt :: UGenSpec -> UGenSpec
   replaceIt ugspec = if isBitNot ugspec
      then UGenSpec
         "BinaryOpUGen"
         (Literal._uGenSpec_calcRate ugspec)
         (Literal._uGenSpec_inputs ugspec <>
            [InputSpec_Constant negOneLoc])
         (Literal._uGenSpec_outputs ugspec)
         (biOpToSpecialI BitXor)
      else ugspec

getSDHashName :: SynthDef a -> ByteString
getSDHashName theSD =
   "vivid_" <> (BS8.pack . show . hash) theSD

{-
-- Anyone, write it for me if you wanna!:
literalToSD :: Literal.SynthDef -> SD
literalToSD =
-}

gatherConstants :: [(Int, UGen)] -> [Float]
gatherConstants ugens =
   nub [ x | Constant x <- concatMap (_ugenIns . snd) ugens]

makeUGenSpecs :: [(ByteString, Float)] -> [(Int, UGen)] -> [Literal.UGenSpec]
makeUGenSpecs params ugens = case params of
   [] -> rest
   _ -> control : rest
 where
   control = UGenSpec
      (BS8.pack "Control")
      KR
      []
      (replicate ((length::[a]->Int) params) (OutputSpec KR))
      0

   rest = map makeSpec ugens

   makeSpec :: (Int, UGen) -> UGenSpec
   makeSpec (_, UGen name calcRate ins numOuts) =
      let (theName, specialIndex) = case name of
             UGName_S s -> (s, 0)
             UGName_U uop -> (BS8.pack "UnaryOpUGen", uOpToSpecialI uop)
             UGName_B biop -> (BS8.pack "BinaryOpUGen", biOpToSpecialI biop)
      in UGenSpec
            theName
            calcRate
            ((flip map) ins $ \case
                Constant x -> InputSpec_Constant $ fromIntegral $ fromJust $
                   elemIndex x $ gatherConstants ugens
                UGOut ugenId outputNum ->
                   let inputPosition =
                           -- If there are any params, there's a "Control" in
                           -- the 0th position:
                          toEnum ugenId + case params of { [] -> 0 ; _ -> 1 }
                   in InputSpec_UGen inputPosition outputNum
                Param s -> InputSpec_UGen 0 (indexOfName params s)
                )
            (replicate numOuts (OutputSpec calcRate))
            specialIndex

 -- invariant: strings are unique:
indexOfName :: (Eq a) => [(ByteString, a)] -> ByteString -> Int32
-- In the future: add levens(t|h)ein distance "did you mean?:"
indexOfName haystack key =
   let foo = case find ((==key) . fst) haystack of
         Nothing -> error $ "missing param: " <> show key
         Just x -> x
   in fromIntegral $ fromJust $ (flip elemIndex) haystack $ foo

getFreshUGenGraphId :: SDBody' args Int
getFreshUGenGraphId = do
   (i:ds, synthDef, argList) <- get
   put (ds, synthDef, argList)
   return i

-- | Alias for 'addMonoUGen'
addUGen :: UGen -> SDBody' args Signal
addUGen = addMonoUGen

-- | Add a unit generator with one output
addMonoUGen :: UGen -> SDBody' args Signal
addMonoUGen ugen = addPolyUGen ugen >>= \case
   [x] -> return x
   foo -> error $ "that ugen's not mono!: " <>   show ugen <>  show foo

-- | Polyphonic -- returns a list of 'Signal's.
--   In the future this might be a tuple instead of a list
addPolyUGen :: UGen -> SDBody' args [Signal]
addPolyUGen ugen = addPolyUGen' $ ugen

addPolyUGen' :: UGen -> SDBody' args [Signal]
addPolyUGen' ugen = do
   anId <- getFreshUGenGraphId
   modify . (\f (a,b,c)->(a,f b,c)) $ \synthDef -> synthDef { _sdUGens =
      Map.unionWith (\_ -> error "dammit keying broken") (_sdUGens synthDef) $
         Map.singleton anId ugen
      }
   return $ map (UGOut anId) [0.. toEnum (_ugenNumOuts ugen - 1)]

-- | Define a Synth Definition
sd :: VarList argList => argList -> SDBody' (InnerVars argList) [Signal] -> SynthDef (InnerVars argList)
sd params theState =
   makeSynthDef SDName_Hash params theState

-- | Define a Synth Definition and give it a name you can refer to from e.g. sclang
sdNamed :: VarList argList => String -> argList -> SDBody' (InnerVars argList) [Signal] -> SynthDef (InnerVars argList)
sdNamed name params theState =
   makeSynthDef (SDName_Named $ BS8.pack name) params theState

makeSynthDef :: VarList argList => SDName -> argList -> SDBody' (InnerVars argList) [Signal] -> SynthDef (InnerVars argList)
makeSynthDef name params theState =
   let theSD = SynthDef name (map (first BS8.pack) paramList) Map.empty
       (paramList, argSet) = makeTypedVarList params
   in (\(_,b,_)->b) $ execState theState $
         ({- id supply: -} [0 :: Int ..], theSD, argSet)

-- | Set the calculation rate of a UGen
-- 
--   e.g.
-- 
--   @
-- play $ do
--    s0 <- 1 ~+ (lfSaw (freq_ 1) ? KR)
--    s1 <- 0.1 ~* lfSaw (freq_ $ 220 ~* s0)
--    out 0 [s1, s1]
-- @
-- 
--   Mnemonic: \"?\" is like thinking
-- 
--   In the future, the representation of calculation rates may change
(?) :: SDBody' args Signal -> CalculationRate -> SDBody' args Signal
(?) i calcRate = do
   i' <- i
   case i' of
      UGOut ugId _o -> modify $ (\f (a,b,c)->(a,f b,c)) $ \synthDef ->
         let ugs = _sdUGens synthDef
             updatedUGens :: Map Int UGen
             updatedUGens = case Map.lookup ugId ugs of
                Nothing -> error "ugen id not found"
                Just UGen{} ->
                   Map.adjust (\ug -> ug { _ugenCalculationRate = calcRate }) ugId ugs
         in synthDef { _sdUGens = updatedUGens }
      _ -> return ()
   return i'

getCalcRate :: Signal -> SDBody' args CalculationRate
getCalcRate (Constant _) = return IR
getCalcRate (Param _) = return KR
getCalcRate (UGOut theUG _) = do
   -- Note: this assumes updates to the ugen graph are only appends
   -- (so don't break that invariant if you build your own graph by hand!):
   (_, ugenGraph, _) <- get
   case Map.lookup theUG (_sdUGens ugenGraph) of
      Just ug -> return $ _ugenCalculationRate ug
      Nothing -> error "that output isn't in the graph!"


-- | Like 'Vivid.SCServer.shrinkNodeArgs' but for 'SynthDef's
shrinkSDArgs :: Subset new old => SynthDef old -> SynthDef new
shrinkSDArgs (SynthDef a b c) = SynthDef a b c



encodeSD :: SynthDef a -> ByteString
encodeSD =
   encodeSynthDefFile . SynthDefFile . (:[]) . sdToLiteral

-- | This is the hash of the UGen graph and params, but not the name!
--   So (re)naming a SynthDef will not change its hash.
instance Hashable (SynthDef a) where
   hashWithSalt salt (SynthDef _name params ugens) =
      hashWithSalt salt . encodeSD $
         SynthDef (SDName_Named "VIVID FTW") params ugens
