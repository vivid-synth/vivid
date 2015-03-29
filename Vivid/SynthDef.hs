-- | Synth Definitions in SuperCollider are how you define the way synths should sound
--   -- you describe parameters and a graph of sound generators, add them to the server
--   with 'defineSD', and then create instances of the Synth Definition (called "synths"),
--   which each play separately. You can set parameters of the synth at any time while
--   they're playing
-- 
--   Usually, you shouldn't be making 'SynthDef's explicitly -- there's a state monad
--   'SDState' which lets you construct synthdefs like so:
-- 
--   @
--   test :: SynthDef
--   test = 'sdNamed' \"testSynthDef\" [(\"note\", 0)] $ do
--      s <- 0.1 'Vivid.UGens.~*' 'Vivid.UGens.sinOsc' (Freq $ 'Vivid.UGens.midiCPS' \"note\")
--      out 0 [s, s]
--   @
-- 
--   You then optionally explicitly send the synth definition to the SC server with
-- 
--   >>> defineSD test
-- 
--   You then create a synth from the synthdef like:
-- 
--   >>> s <- synth "testSynthDef" [("note", 45)]
-- 
--   Or, alternately:
-- 
--   >>> s <- synth test [("note", 45)]
-- 
--   This returns a 'NodeId' which is a reference to the synth, which you can
--   use to e.g. change the params of the running synth with e.g.
-- 
--   >>> set s [("note", 38)]
-- 
--   Then you can free it (stop its playing) with
-- 
--   >>> free s

{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE NoRebindableSyntax #-}

-- {-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Vivid.SynthDef (
  -- * Synth actions

    synth
  , set
  , free

  -- * Synth Definition Construction

  , SynthDef(..)
  , UGen(..)
  , addUGen
  , addMonoUGen
  , addPolyUGen
  , ToSig(..)
  , ToSigM(..)
  , Signal(..)
--  , SDState
  , encodeSD
  , defineSD
  , sd
  , sdNamed
  , sdPretty
  , (?)
  , play
  , cmdPeriod
  , DoneAction(..)
  , doneActionNum
  , sdLitPretty
  , HasSynthRef
  , sdToLiteral
  -- literalToSD

  , execState

  , getCalcRate

{-
  -- * Type-defaulting stuff
  , fromInteger
  , fromString
  , fromRational
  , int
  , integer
  , i8
  , i16
  , i32
  , string
-}

  -- * Built-in Unit Generator Operations

  , UnaryOp(..)
  , uOpToSpecialI
  , specialIToUOp

  , BinaryOp(..)
  , biOpToSpecialI
  , specialIToBiOp

  , module Vivid.SynthDef.Types
  ) where

import Vivid.OSC (OSC(..), OSCDatum(..))
import Vivid.SCServer
import Vivid.SynthDef.CrazyTypes
import Vivid.SynthDef.Literally as Literal
import Vivid.SynthDef.Types

import Control.Applicative
import Control.Arrow (first, second)
import Control.Concurrent.STM
import Control.Monad.State
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import Data.Hashable
import Data.Int
import Data.List (nub, elemIndex, find) -- , sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set

-- once upon a time, we used -XRebindableSyntax to do Float defaulting instead of -XIncoherentInstances -- this is the machinery for that to work:
{-
import Prelude hiding (Num(..), fromRational) -- so i can do Float defaulting
import qualified Prelude as N
import qualified Data.String (fromString)

fromInteger :: Integer -> Float
fromInteger = realToFrac

fromRational :: Rational -> Float
fromRational = N.fromRational

int :: Float -> Int
int = fromEnum

integer :: Float -> Integer
integer = toInteger . fromEnum

i8 :: Float -> Int8
i8 = fromIntegral . int

i16 :: Float -> Int16
i16 = fromIntegral . int

i32 :: Float -> Int32
i32 = fromIntegral . int

fromString :: String -> ByteString
fromString = Data.String.fromString

string :: ByteString -> String
string = BS8.unpack
-}

sdPretty :: SynthDef -> String
sdPretty synthDef = unlines $ [
     "Name: " <> show (_sdName synthDef)
   , "Args: " <> show (_sdParams synthDef)
   , "UGens: "
   ] <> map show (Map.toAscList (_sdUGens synthDef))


data DoneAction
   = DoNothing
   | FreeEnclosing
 deriving (Show, Eq)

doneActionNum :: DoneAction -> Float
doneActionNum = \case
   DoNothing -> 0
   FreeEnclosing -> 2

uOpToSpecialI :: UnaryOp -> Int16
uOpToSpecialI uop = toEnum . fromEnum $ uop

specialIToUOp :: Int16 -> UnaryOp
specialIToUOp specialI = toEnum . fromEnum $ specialI

biOpToSpecialI :: BinaryOp -> Int16
biOpToSpecialI theBiOp = toEnum . fromEnum $ theBiOp

specialIToBiOp :: Int16 -> BinaryOp
specialIToBiOp theBiOp = toEnum . fromEnum $ theBiOp

--invariants (to check):
-- param names don't clash
-- graph is real and acyclic
-- no "dangling" pieces -- sign that something's wrong
-- params are all used, and the ones that're used in the graph all exist


sdToLiteral :: SynthDef -> Literal.LiteralSynthDef
sdToLiteral theSD@(SynthDef name params ugens) =
   LiteralSynthDef
      (case name of
         SDName_Named s -> s
         SDName_Hash -> getSDHashName theSD
         )
      (gatherConstants $ Map.toAscList ugens)
      (map snd params)
      (zipWith (\s i -> ParamName s i) (map fst params) [0..])
      (makeUGenSpecs params $ Map.toAscList ugens)
      []

getSDHashName :: SynthDef -> ByteString
getSDHashName theSD =
   "vivid_" <> (BS8.pack . show . hash) theSD

{-
-- Write it if you wanna:
literalToSD :: Literal.SynthDef -> SD
literalToSD = undefined
-}

encodeSD :: SynthDef -> ByteString
encodeSD = encodeSynthDefFile . SynthDefFile . (:[]) . sdToLiteral

-- | This is the hash of the UGen graph and params, but not the name!
--   So (re)naming a SynthDef will not change its hash.
instance Hashable SynthDef where
   hashWithSalt salt (SynthDef _name params ugens) =
      hashWithSalt salt . encodeSD $
         SynthDef (SDName_Named "VIVID FTW") params ugens

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
      (replicate (length params) (OutputSpec KR))
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
                   let inputPosition = toEnum ugenId + case params of
                          [] -> 0
                          _ -> 1 -- if there are any params, there's a "Control" in
                                 -- the 0th position
                   in InputSpec_UGen inputPosition outputNum
                Param s -> InputSpec_UGen 0 (indexOfName params s)
                )
            (replicate numOuts (OutputSpec calcRate))
            specialIndex

 -- invariant: strings are unique:
indexOfName :: (Eq a) => [(ByteString, a)] -> ByteString -> Int32
-- in the future: add levens(t|h)ein distance "did you mean?:"
indexOfName haystack key =
   let foo = case find ((==key) . fst) haystack of
         Nothing -> error $ "missing param: " <> show key
         Just x -> x
   in fromIntegral $ fromJust $ (flip elemIndex) haystack $ foo

-- | Send a synth definition to be loaded on the SC server
-- 
--   Note that this is sort of optional -- if you don't call it, it'll be called the first time
--   you call 'synth' with the SynthDef
defineSD :: SynthDef -> IO ()
defineSD synthDef =
   defineSDIfNeeded synthDef

defineSDIfNeeded :: SynthDef -> IO ()
defineSDIfNeeded synthDef@(SynthDef name _ _) = do
   let !_ = scServerState
   hasBeenDefined <- (((name, hash synthDef) `Set.member`) <$>) $
      readTVarIO (scServer_definedSDs scServerState)
   unless hasBeenDefined $ do
      callAndWaitForDone $ OSC (BS8.pack "/d_recv") [
           OSC_B $ encodeSD synthDef
         , OSC_I 0
         ]
      atomically $ modifyTVar (scServer_definedSDs scServerState) $
         ((name, hash synthDef) `Set.insert`)

getFreshUGenGraphId :: SDState Int
getFreshUGenGraphId = do
   (i:ds, synthDef) <- get
   put (ds, synthDef)
   return i

-- | Alias for 'addMonoUGen'
addUGen :: UGen -> SDState Signal
addUGen = addMonoUGen

-- | Add a unit generator with one output
addMonoUGen :: UGen -> SDState Signal
addMonoUGen ugen = addPolyUGen ugen >>= \case
   [x] -> return x
   foo -> error $ "that ugen's not mono!: " <> show ugen <> show foo

-- | Polyphonic -- returns a list of 'Signal's.
--   In the future this might be a tuple instead of a list
addPolyUGen :: UGen -> SDState [Signal]
addPolyUGen ugen = do
   anId <- getFreshUGenGraphId
   modify . second $ \synthDef -> synthDef { _sdUGens =
      Map.unionWith (\_ -> error "dammit keying broken") (_sdUGens synthDef) $
         Map.singleton anId ugen
      }
   return $ map (UGOut anId) [0.. toEnum (_ugenNumOuts ugen - 1)]

-- | Define a Synth Definition
sd :: [(String, Float)] -> SDState x -> SynthDef
sd params theState =
   makeSynthDef SDName_Hash params theState

-- | Define a Synth Definition and give it a name you can refer to from e.g. sclang
sdNamed :: String -> [(String, Float)] -> SDState x -> SynthDef
sdNamed name params theState =
   makeSynthDef (SDName_Named $ BS8.pack name) params theState

makeSynthDef :: SDName -> [(String, Float)] -> SDState x -> SynthDef
makeSynthDef name params theState =
   let theSD = SynthDef name (map (first BS8.pack) params) Map.empty
   in snd $ execState theState ({- id supply: -} [0 :: Int ..], theSD)


-- | Set the calculation rate of a UGen
-- 
--   e.g.
-- 
--   @
-- play $ do
--    s0 <- 1 ~+ (lfSaw (Freq 1) ? KR)
--    s1 <- 0.1 ~* lfSaw (Freq $ 220 ~* s0)
--    out 0 [s1, s1]
-- @
-- 
--   Mnemonic: \"?\" is like thinking
-- 
--   In the future, the representation of calculation rates definitely may change
(?) :: SDState Signal -> CalculationRate -> SDState Signal
(?) i calcRate = do
   i' <- i
   case i' of
      UGOut ugId _o -> modify $ second $ \synthDef ->
         let ugs = _sdUGens synthDef
             updatedUGens :: Map Int UGen
             updatedUGens = case Map.lookup ugId ugs of
                Nothing -> error "ugen id not found"
                Just UGen{} ->
                   Map.adjust (\ug -> ug { _ugenCalculationRate = calcRate }) ugId ugs
         in synthDef { _sdUGens = updatedUGens }
      _ -> return ()
   return i'

getCalcRate :: Signal -> SDState CalculationRate
getCalcRate (Constant _) = return IR
getCalcRate (Param _) = return KR
getCalcRate (UGOut theUG _) = do
   -- Note: this assumes updates to the ugen graph are only appends
   -- (so don't break that invariant if you build your own graph by hand!):
   (_, ugenGraph) <- get
   case Map.lookup theUG (_sdUGens ugenGraph) of
      Just ug -> return $ _ugenCalculationRate ug
      Nothing -> error "that output isn't in the graph!"

-- | Given a UGen graph, just start playing it right away.
-- 
--   e.g.
-- 
--   > play $ do
--   >    s <- 0.2 ~* lpf (In whiteNoise) (Freq 440)
--   >    out 0 [s, s]
play :: SDState a -> IO NodeId
play x = do
   let graphWithOut = x
   let sdWithOut = sd [] graphWithOut
   synth sdWithOut []

sdLitPretty :: Literal.LiteralSynthDef -> String
sdLitPretty synthDef = mconcat [
    "Constants: ", show $ _synthDefConstants synthDef
  , "\n"
  , mconcat$
      (flip map) (zip [0..] (Literal._synthDefUGens synthDef)) $ \(i,ug) -> mconcat [
                show i <> " " <> show (_uGenSpec_name ug) <> " - " <> show (_uGenSpec_calcRate ug)
               ,"\n"
               ,mconcat $ map ((<>"\n") . ("  "<>) . showInputSpec) $ _uGenSpec_inputs ug
               ,case BS8.unpack (_uGenSpec_name ug) of
                   "UnaryOpUGen" -> mconcat [ "  "
                      , show ( specialIToUOp (_uGenSpec_specialIndex ug))
                      , "\n" ]
                   "BinaryOpUGen" ->
                      "  " <> show (specialIToBiOp (_uGenSpec_specialIndex ug)) <> "\n"

                   _ -> ""
               ]
  ]
 where
   showInputSpec :: InputSpec -> String
   showInputSpec (InputSpec_Constant constantIndex) = mconcat [
       "Constant: "
      ,show $ (_synthDefConstants synthDef) !! fromEnum constantIndex
      ," (index ", show constantIndex, ")"
      ]
   showInputSpec x = show x

-- | Immediately stop a synth playing
-- 
--   This can create a \"clipping\" artifact if the sound goes from a high
--   amplitude to 0 in an instant -- you can avoid that with e.g.
--   'Vivid.UGens.lag'
free :: NodeId -> IO ()
free (NodeId nodeId) =
   call $ OSC (BS8.pack "/n_free") [ OSC_I nodeId ]

-- | Set the given parameters of a running synth
-- 
--   e.g.
-- 
--   >>> let setTest = sd [("pan", 0.5)] $ out 0 =<< pan2 (In $ 0.1 ~* whiteNoise) (Pos "pan")
--   >>> s <- synth setTest []
--   >>> set s [("pan", -0.5)]
-- 
--   Any parameters not referred to will be unaffected, and any you specify that don't exist
--   will be (silently) ignored
set :: NodeId -> [(String, Float)] -> IO ()
set (NodeId nodeId) params =
   call $ OSC (BS8.pack "/n_set") $ OSC_I nodeId : paramList
 where
   paramList :: [OSCDatum]
   paramList = concatMap (\(k,v)->[OSC_S k,OSC_F v]) $
      map (first BS8.pack) params

-- | Create a real live music-playing synth from a boring, dead SynthDef.
-- 
--   If you haven't defined the SynthDef on the server, this will do it automatically
--   (Note that this may cause jitters in musical timing)
-- 
--   Uses 'HasSynthRef' so that given...
-- 
--   >>> let foo = sdNamed "foo" [] $ out 0 [0.1 ~* whiteNoise]
-- 
--   ...you can create a synth either with...
-- 
--   >>> synth "foo" []
-- 
--   ...or...
-- 
--   >>> synth foo []
-- 
--   Careful!: The SC server doesn't keep track of your nodes for you,
--   so if you do something like...
-- 
--   >>> s <- synth "someSynth" []
--   >>> s <- synth "oops" []           -- 's' is overwritten
-- 
--   ...you've got no way to refer to the first synth you've created, and if you
--   want to stop it you have to 'cmdPeriod'
synth :: (HasSynthRef a) => a -> [(String, Float)] -> IO NodeId
synth refHolder params = do
   case getSynthRef refHolder of
      Left _ -> return ()
      Right aSD -> defineSDIfNeeded aSD

   nodeId@(NodeId nn) <- newNodeId
   let synthName = case getSynthRef refHolder of
        Left sn -> sn
        Right (SynthDef (SDName_Named n) _ _) -> n
        Right theSD@(SynthDef SDName_Hash _ _) -> getSDHashName theSD
   call $ OSC (BS8.pack "/s_new") $ [
        OSC_S $ synthName, OSC_I nn
      , OSC_I 0
      , OSC_I 1
      ] <> paramList
   return nodeId
 where
   paramList :: [OSCDatum]
   paramList = concatMap (\(k, v) -> [OSC_S k, OSC_F v]) $
      map (first BS8.pack) params
