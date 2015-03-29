-- |  __You probably don't need to use this directly__ -- use "Vivid.SynthDef" instead
-- 
--   This is a representation of how SynthDefs are sent over the wire, as described in the
--   < http://doc.sccode.org/Reference/Synth-Definition-File-Format.html Synth Definition File Format >
--   helpfile.
-- 

{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Vivid.SynthDef.Literally (
     LiteralSynthDef(..)
   , encodeSynthDefFile
   , decodeSynthDefFile

   , UGenSpec(..)
   , InputSpec(..)
   , ParamName(..)
   , SynthDefFile(..)
   , OutputSpec(..)
   ) where

import Vivid.SynthDef.Types
import Vivid.OSC.Util (floatToWord, wordToFloat)

import Control.Arrow (first)
import Control.Monad (when)
import Data.Binary (decode, encode)
import qualified Data.ByteString as B
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Int
import Data.List.Split (chunksOf)
import Data.Monoid

data LiteralSynthDef
   = LiteralSynthDef {
    _synthDefName :: ByteString -- pstring: "a pascal format string: a byte giving the length followed by that many bytes"
   ,_synthDefConstants :: [Float]
   ,_synthDefParameters :: [Float] -- Initial values
   ,_synthDefParamNames :: [ParamName]
   ,_synthDefUGens :: [UGenSpec]
   ,_synthDefVariants :: [VariantSpec]
   }
 deriving (Show)

data SynthDefFile = SynthDefFile [LiteralSynthDef]
 deriving (Show)

decodeSynthDefFile :: ByteString -> IO SynthDefFile
decodeSynthDefFile blob = do
   let (top, rest) = BS.splitAt 4 blob
   let (fileVersion :: Int32, rest2) =
          first (decode . BSL.fromStrict) $ BS.splitAt 4 rest

   when (top /= "SCgf" || fileVersion /= 2) $
      error $ "screwed up synthdef file " <> show top <> show fileVersion

   let (numberOfSynthDefs :: Int16, rest3) =
          first (decode . BSL.fromStrict) $ BS.splitAt 2 rest2

   let (synthDefs, rest4) = getNWith numberOfSynthDefs decodeSynthDef rest3

   if rest4 /= ""
      then error $ "leftover data: " <> show rest4
      else return ()

   return $ SynthDefFile synthDefs

{-
a synth-definition-file is :
int32 - four byte file type id containing the ASCII characters: "SCgf"
int32 - file version, currently 2.
int16 - number of synth definitions in this file (D).
[ synth-definition ] * D
-}

encodeSynthDefFile :: SynthDefFile -> ByteString
encodeSynthDefFile (SynthDefFile synthDefs) = mconcat [
     "SCgf"
   , BSL.toStrict $ encode (2 :: Int32)
   , BSL.toStrict $ encode (toEnum (length synthDefs) :: Int16)
   , mconcat $ map encodeSynthDef synthDefs
   ]

-- Yes, the 'restN's are ugly, yes i could have used a state monad. Don't judge me.
decodeSynthDef :: ByteString -> (LiteralSynthDef, {- rest: -} ByteString)
decodeSynthDef blob =
   let (name :: ByteString, rest) = getPString blob

       (numConstants :: Int32, rest2) = getInt32 rest

       (constants :: [Float], rest3) =
          first (map (wordToFloat . decode . BSL.fromStrict)) $ getN4ByteBlocks numConstants rest2

       (numParams :: Int32, rest4) = getInt32 rest3

       (params :: [Float], rest5) =
          first (map (wordToFloat . decode . BSL.fromStrict)) $ getN4ByteBlocks numParams rest4

       (numParamNames :: Int32, rest6) = getInt32 rest5

       (paramNames :: [ParamName], rest7) = getNWith numParamNames getParamName rest6

       (numUGens :: Int32, rest8) = getInt32 rest7

       (uGens, rest9) = getNWith numUGens getUGenSpec rest8

       (numVariants, rest10) = getInt16 rest9

       (variantSpecs, rest11) = getNWith numVariants (getVariantSpec numParams) rest10

   in (LiteralSynthDef name constants params paramNames uGens variantSpecs, rest11)

{-
a synth-definition is :
pstring - the name of the synth definition
int32 - number of constants (K)
[float32] * K - constant values
int32 - number of parameters (P)
[float32] * P - initial parameter values
int32 - number of parameter names (N)
[ param-name ] * N
int32 - number of unit generators (U)
[ ugen-spec ] * U
int16 - number of variants (V)
[ variant-spec ] * V
-}

encodeSynthDef :: LiteralSynthDef -> ByteString
encodeSynthDef (LiteralSynthDef name constants params paramNames uGenSpecs variants) = mconcat [
     encodePString name
   , BSL.toStrict $ encode (toEnum (length constants) :: Int32)
   , mconcat $ map (BSL.toStrict . encode . floatToWord) constants
   , BSL.toStrict $ encode (toEnum (length params) :: Int32)
   , mconcat $ map (BSL.toStrict . encode . floatToWord) params
   , BSL.toStrict $ encode (toEnum (length paramNames) :: Int32)
   , mconcat $ map encodeParamName paramNames
   , BSL.toStrict $ encode (toEnum (length uGenSpecs) :: Int32)
   , mconcat $ map encodeUGenSpec uGenSpecs
   , BSL.toStrict $ encode (toEnum (length variants) :: Int16)
   , mconcat $ map encodeVariantSpec variants
   ]

data ParamName = ParamName {
    _paramName_name :: ByteString
   ,_paramName_indexInParamArray :: Int32
   }
 deriving (Show)

{-
a param-name is :
pstring - the name of the parameter
int32 - its index in the parameter array
-}

getParamName :: ByteString -> (ParamName, ByteString)
getParamName blob =
   let (name, rest) = getPString blob
       (index, rest2) = getInt32 rest
   in (ParamName name index, rest2)

encodeParamName :: ParamName -> ByteString
encodeParamName (ParamName name index) =
   encodePString name <> BSL.toStrict (encode index)

data UGenSpec = UGenSpec {
    _uGenSpec_name :: ByteString
   ,_uGenSpec_calcRate :: CalculationRate
   ,_uGenSpec_inputs :: [InputSpec]
   ,_uGenSpec_outputs :: [OutputSpec]
   ,_uGenSpec_specialIndex :: Int16
   }
 deriving (Show)

{-
a ugen-spec is :
pstring - the name of the SC unit generator class
int8 - calculation rate
int32 - number of inputs (I)
int32 - number of outputs (O)
int16 - special index
[ input-spec ] * I
[ output-spec ] * O
-}

getUGenSpec :: ByteString -> (UGenSpec, {- rest: -} ByteString)
getUGenSpec blob =
   let (name, rest) = getPString blob
       (calcRate :: CalculationRate, rest2) =
          first ((toEnum) . (fromEnum :: Int8 -> Int) . decode . BSL.fromStrict) $
             B.splitAt 1 rest
       (numInputs :: Int32, rest3) = getInt32 rest2
       (numOutputs :: Int32, rest4) = getInt32 rest3

       (specialIndex, rest5) = getInt16 rest4
       (inputSpecs, rest6) = getNWith numInputs getInputSpec rest5
       (outputSpecs, rest7) = getNWith numOutputs getOutputSpec rest6

   in (UGenSpec name calcRate inputSpecs outputSpecs specialIndex, rest7)

encodeUGenSpec :: UGenSpec -> ByteString
encodeUGenSpec (UGenSpec name calcRate inputSpecs outputSpecs specialIndex) = mconcat [
    encodePString name
   ,BSL.toStrict $ encode $ (toEnum (fromEnum calcRate) :: Int8)
   ,BSL.toStrict $ encode $ (toEnum (length inputSpecs) :: Int32)
   ,BSL.toStrict $ encode $ (toEnum (length outputSpecs) :: Int32)
   ,BSL.toStrict $ encode specialIndex
   ,mconcat $ map encodeInputSpec inputSpecs
   ,mconcat $ map encodeOutputSpec outputSpecs
   ]

data InputSpec
   = InputSpec_UGen {
    _inputSpec_uGen_index :: Int32
   ,_inputSpec_uGen_outputIndex :: Int32
   }
   | InputSpec_Constant {
    _inputSpec_constant_index :: Int32
   }
 deriving (Show, Read, Eq)

{-
an input-spec is :
int32 - index of unit generator or -1 for a constant
if (unit generator index == -1) :
int32 - index of constant
else :
int32 - index of unit generator output
-}

getInputSpec :: ByteString -> (InputSpec, ByteString)
getInputSpec blob =
   let (stuffForThis, rest) = BS.splitAt 8 blob
       (one :: Int32, two :: Int32) = (\(a,b) -> ((decode . BSL.fromStrict) a, (decode . BSL.fromStrict) b)) $ BS.splitAt 4 stuffForThis
       spec = case one of
          -1 -> InputSpec_Constant two
          n | n > -1 -> InputSpec_UGen one two
          _ -> error "bad number"
   in (spec, rest)

encodeInputSpec :: InputSpec -> ByteString
encodeInputSpec inputSpec = mconcat $ map (BSL.toStrict . encode) $ encodeInputSpec' inputSpec
 where
   encodeInputSpec' :: InputSpec -> [Int32]
   encodeInputSpec' (InputSpec_Constant i) = [ (-1), i ]
   encodeInputSpec' (InputSpec_UGen i oI) = [ i, oI ]

data OutputSpec = OutputSpec { _outputSpec_calcRate :: CalculationRate }
 deriving (Show, Read, Eq)

{-
an output-spec is :
int8 - calculation rate
-}

getOutputSpec :: ByteString -> (OutputSpec, ByteString)
getOutputSpec blob =
      first (OutputSpec . toEnum . (fromEnum :: Int8 -> Int) . decode . BSL.fromStrict) $
         BS.splitAt 1 blob

encodeOutputSpec :: OutputSpec -> ByteString
encodeOutputSpec (OutputSpec calcRate) =
   BSL.toStrict $ encode $ (toEnum (fromEnum calcRate) :: Int8)

data VariantSpec
   = VariantSpec {
    _variantSpec_name :: ByteString
   ,_variantSpec_initialParamVals :: [Float] -- float32
   }
 deriving (Show)

{-
a variant-spec is :
pstring - the name of the variant
[float32] * P - variant initial parameter values
-}

getVariantSpec :: Int32 -> ByteString -> (VariantSpec, ByteString)
getVariantSpec numParams blob =
   let (name, rest) = getPString blob
       (initialParamVals :: [Float], rest2) = first (map (decode . BSL.fromStrict)) $ getN4ByteBlocks numParams rest
   in (VariantSpec name initialParamVals, rest2)

encodeVariantSpec :: VariantSpec -> ByteString
encodeVariantSpec (VariantSpec name initialParamVals) =
   encodePString name <> mconcat (map (BSL.toStrict . encode . floatToWord) initialParamVals)

--- helpers:
getPString :: ByteString -> (ByteString, {- rest: -} ByteString)
getPString blob = first (BS.drop 1) $
   BS.splitAt (fromEnum (BS.head blob) + 1) blob

encodePString :: ByteString -> ByteString
encodePString s = toEnum (BS.length s) `BS.cons` s

getNWith :: (Integral i) => i -> (ByteString -> (a, ByteString)) -> ByteString -> ([a], ByteString)
getNWith 0 _ rest = ([], rest)
getNWith n f rest =
   let (head1, rest2) = f rest
       (head2, rest3) = getNWith (n - 1) f rest2
   in (head1 : head2, rest3)
   
getInt32 :: ByteString -> (Int32, ByteString)
getInt32 blob = first (decode . BSL.fromStrict) $ B.splitAt 4 blob

getInt16 :: ByteString -> (Int16, ByteString)
getInt16 blob = first (decode . BSL.fromStrict) $ B.splitAt 2 blob

getN4ByteBlocks :: Int32 -> ByteString -> ([ByteString], ByteString)
getN4ByteBlocks numBlocks blob =
   first (map (BS8.pack) . chunksOf 4 . BS8.unpack) $
             B.splitAt (4 * fromEnum numBlocks) blob
