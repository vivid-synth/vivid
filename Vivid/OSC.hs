-- | __You probably don't need to use this directly__
-- 
--   Representation of Open Sound Control data

{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Vivid.OSC (
     OSC(..)
   , OSCDatum(..)

   , encodeOSC
   , decodeOSC
   ) where

import Vivid.OSC.Util

import Control.DeepSeq
import Data.Binary (encode, decode)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int32) -- , Int16, Int8)
import Data.Monoid

-- | An OSC message, e.g.
-- 
--   > OSC "/n_free" [OSC_I 42]
data OSC
   = OSC ByteString [OSCDatum]
 deriving (Show, Read, Eq)

data OSCDatum
   = OSC_I Int32
   | OSC_S ByteString
   | OSC_F Float
{-
   | OSC_I8 Int8
   | OSC_I16 Int16
-}
   | OSC_B ByteString
 deriving (Show, Read, Eq)

-- formerly known as 'someShit':
encodeOSC :: OSC -> ByteString
encodeOSC (OSC url args) = BSL.toStrict $ BSL.concat $ [
    encodeDatum (OSC_S url)
   ,encodeDatum (OSC_S ("," <> BS.concat (map toTypeChar args)))
   ] <> map encodeDatum args
  where
    toTypeChar (OSC_I _) = "i"
    toTypeChar (OSC_S _) = "s"
    toTypeChar (OSC_F _) = "f"
    toTypeChar (OSC_B _) = "b"

encodeDatum :: OSCDatum -> BSL.ByteString
encodeDatum (OSC_I i) = encode i
encodeDatum (OSC_S s) = BSL.fromStrict $
   s <> BS.replicate (align (BS.length s + 1) + 1) 0
encodeDatum (OSC_F f) = (encode . floatToWord) f
encodeDatum (OSC_B b) = mconcat [
    -- 4 bytes which describe the size of the blob:
     encode (fromIntegral (BS.length b) :: Int32)
    -- the blob itself:
   , BSL.fromStrict b
    -- padding:
   , BSL.fromStrict (BS8.pack (replicate  (align (BS.length b)) '\NUL'))
   ]

decodeDatumWithPadding :: Char -> ByteString -> OSCDatum
decodeDatumWithPadding 'i' b =
   OSC_I (decode $ BSL.fromStrict b)
decodeDatumWithPadding 'f' b =
   OSC_F (wordToFloat . decode $ BSL.fromStrict b)
decodeDatumWithPadding 's' b =
   OSC_S  $ BS.take (numBytesWithoutPadding 's' b)  b
decodeDatumWithPadding 'b' b =
   OSC_B $ BS.take (numBytesWithoutPadding 'b' b) $ BS.drop 4 b
decodeDatumWithPadding c b =
   error $ "unknown character " <> show c <> ": " <> show b

numBytesWithoutPadding :: Char -> ByteString -> Int
numBytesWithoutPadding 'i' _ = 4
numBytesWithoutPadding 'f' _ = 4
numBytesWithoutPadding 's' b = case BS.elemIndex 0 $ b of
   Just x -> fromIntegral x
   Nothing -> error $ "weirdness on " <> show b
numBytesWithoutPadding 'b' b = fromIntegral $
   (decode $ BSL.fromStrict $ BS.take 4 b :: Int32)
numBytesWithoutPadding c b =
   error $ "unknown character " <> show c <> ": " <> show b

numBytesWithPadding :: Char -> ByteString -> Int
numBytesWithPadding 'i' _ = 4
numBytesWithPadding 'f' _ = 4
numBytesWithPadding 's' b =
   let n = (numBytesWithoutPadding 's' b) + 1
   in n + (align n)
numBytesWithPadding 'b' b =
   let n = numBytesWithoutPadding 'b' b
   in n + align n + 4
numBytesWithPadding c b =
   error $ "unknown character " <> show c <> ": " <> show b

decodeOSCData :: [Char] -> ByteString -> [OSCDatum]
decodeOSCData [] "" = []
decodeOSCData [] leftover = error $ "leftover bytes: " <> show leftover
decodeOSCData (t:ypes) blob =
   (:) datum
       (decodeOSCData ypes (BS.drop (numBytesWithPadding t blob) blob))
 where
   datum = decodeDatumWithPadding t thisBlob
   thisBlob = BS.take (numBytesWithPadding t blob) blob

decodeOSC :: ByteString -> OSC
decodeOSC b =
   let sizeOfURL = numBytesWithoutPadding 's' b
       storageOfURL = numBytesWithPadding 's' b

       url = BS.take sizeOfURL b

       -- typeDesc is like ",issif"
       sizeOfTypeDesc = numBytesWithoutPadding 's' $ BS.drop storageOfURL b
       storageOfTypeDesc = numBytesWithPadding 's' $ BS.drop storageOfURL b
       (',':typeDesc) = BS8.unpack $ BS.take sizeOfTypeDesc $
          BS.drop storageOfURL b

       rest = BS.drop (storageOfURL + storageOfTypeDesc) $ b

   in OSC url $ decodeOSCData typeDesc rest

instance NFData OSCDatum where
   rnf (OSC_I x) = rnf x
   rnf (OSC_F x) = rnf x
   rnf (OSC_S x) = rnf x
{-
   rnf (OSC_I8 x) = rnf x
   rnf (OSC_I16 x) = rnf x
-}
   rnf (OSC_B x) = rnf x
