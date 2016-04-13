-- | __You probably don't need to use this directly__
-- 
--   Representation of Open Sound Control data

{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.OSC (
     OSC(..)
   , OSCDatum(..)

   , encodeOSC
   , decodeOSC

   , Timestamp(..)
   , OSCBundle(..)

   , encodeOSCBundle
   , encodeOSCBundles
   -- Someone implement this for me plz!:
   -- , decodeBundle

   , encodeTimestamp
   , utcToTimestamp

   , addSecs
   , diffTimestamps

   , initTreeCommand
   ) where

import Vivid.OSC.Util

-- import Control.DeepSeq (NFData, rnf)
import Data.Binary (encode, decode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (length, drop, take, elemIndex, replicate, concat)
import qualified Data.ByteString.Char8 as BS8 (unpack, pack)
import qualified Data.ByteString.Lazy as BSL (toStrict, fromStrict, concat, ByteString)
import Data.Int (Int32)
import qualified Data.List as L
import Data.Monoid
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime, diffUTCTime)
import Data.Word

-- | An OSC message, e.g.
-- 
--   > OSC "/n_free" [OSC_I 42]
data OSC
   = OSC ByteString [OSCDatum]
 deriving (Show, Read, Eq, Ord)

data OSCDatum
   = OSC_I Int32
   | OSC_S ByteString
   | OSC_F Float
   | OSC_D Double -- ^ This isn't a base type in the OSC standard but the response message from "/status" uses it...
{-
   | OSC_I8 Int8
   | OSC_I16 Int16
-}
   | OSC_B ByteString
   -- OSC_T Timestamp -- ^ From the OSC 1.1 spec
 deriving (Show, Read, Eq, Ord)

-- | This is stored as the number of seconds since Jan 1 1900. You can get
--   it with 'Vivid.Actions.Class.getTime'
newtype Timestamp = Timestamp Double
   deriving (Show, Read, Eq, Ord)

data OSCBundle
   = OSCBundle Timestamp [Either ByteString OSC]
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
    toTypeChar (OSC_D _) = "d"

encodeDatum :: OSCDatum -> BSL.ByteString
encodeDatum (OSC_I i) = encode i
encodeDatum (OSC_S s) = BSL.fromStrict $
   s <> BS.replicate (align (BS.length s + 1) + 1) 0
encodeDatum (OSC_F f) = (encode . floatToWord) f
encodeDatum (OSC_D d) = (encode . doubleToWord) d
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
decodeDatumWithPadding 'd' b =
   OSC_D (wordToDouble . decode $ BSL.fromStrict b)
decodeDatumWithPadding c b =
   error $ "unknown character " <> show c <> ": " <> show b

numBytesWithoutPadding :: Char -> ByteString -> Int
numBytesWithoutPadding 'i' _ = 4
numBytesWithoutPadding 'f' _ = 4
numBytesWithoutPadding 'd' _ = 8
numBytesWithoutPadding 's' b = case BS.elemIndex 0 $ b of
   Just x -> fromIntegral x
   Nothing -> error $ "weirdness on " <> show b
numBytesWithoutPadding 'b' b = fromIntegral $
   (decode $ BSL.fromStrict $ BS.take 4 b :: Int32)
numBytesWithoutPadding c b =
   error $ "vivid: unknown OSC character " <> show c <> ": " <> show b

numBytesWithPadding :: Char -> ByteString -> Int
numBytesWithPadding 'i' _ = 4
numBytesWithPadding 'f' _ = 4
numBytesWithPadding 'd' _ = 8
numBytesWithPadding 's' b =
   let n = (numBytesWithoutPadding 's' b) + 1
   in n + (align n)
numBytesWithPadding 'b' b =
   let n = numBytesWithoutPadding 'b' b
   in n + align n + 4
numBytesWithPadding c b =
   error $ "vivid: unknown OSC character " <> show c <> ": " <> show b

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

encodeOSCBundle :: OSCBundle -> ByteString
encodeOSCBundle (OSCBundle time messages) = mconcat [
     "#bundle\NUL"
   , encodeTimestamp time
   , (mconcat . map (addLength . either id encodeOSC)) messages
   ]

-- | Encode OSC bundles, specifically for NRT synthesis.
--   (It's more than just \"mconcat . map 'encodeOSCBundle'\").
-- 
--   Note also that the last action is when the song ends - so if you want
--   e.g. a note to hold at the end you need to add a "wait"
encodeOSCBundles :: [OSCBundle] -> ByteString
encodeOSCBundles bundles =
   mconcat . map (addLength . encodeOSCBundle) $ withEnd
 where
   sortedBundles :: [OSCBundle]
   sortedBundles =
      L.sortBy (\(OSCBundle t0 _) (OSCBundle t1 _) -> compare t0 t1) bundles
   sortedBundlesWithDefinitionsFirst :: [OSCBundle]
   sortedBundlesWithDefinitionsFirst =
      map putDefinitionsFirst joinedByTime
    where
      -- Note we aren't assuming there aren't bundles with the same timestamp.
      -- (Which isnt an issue if we got the bundles with 'runNRT', but it's good
      -- to check):
      joinedByTime :: [OSCBundle]
      joinedByTime =
         (flip map) groupedByTime $ \case
            as@(OSCBundle t _:_) ->
               OSCBundle t (concatMap (\(OSCBundle _ a) -> a) as)
            [] -> error "Should be impossible"
      groupedByTime :: [[OSCBundle]]
      groupedByTime =
         L.groupBy (\(OSCBundle t0 _) (OSCBundle t1 _) -> t1 == t0) sortedBundles

      -- If there are "/d_recv" actions and other actions at the same timestamp, we
      -- put the "/d_recv"s before the other actions:
      putDefinitionsFirst :: OSCBundle -> OSCBundle
      putDefinitionsFirst (OSCBundle t actions) = OSCBundle t $ (\(a,b)->a<>b) $
         (flip L.partition) actions $ \case
            Right (OSC "/d_recv" _) -> True
            _ -> False
   lastTimestamp = (\(OSCBundle t _) ->t) $ last sortedBundles

   withEnd = mconcat [
       [OSCBundle (Timestamp 0) [Right initTreeCommand]]
      ,sortedBundlesWithDefinitionsFirst
      ,[OSCBundle lastTimestamp [Right $ OSC "" []]]
      ]

addLength :: ByteString -> ByteString
addLength bs =
   BSL.toStrict (encode (toEnum (BS.length bs) :: Word32)) <> bs

encodeTimestamp :: Timestamp -> ByteString
encodeTimestamp (Timestamp time) =
   BSL.toStrict $ encode $ (round (time * 2^(32::Int)) :: Word64)

utcToTimestamp :: UTCTime -> Timestamp
utcToTimestamp utcTime =
   let startOfTheCentury =
          UTCTime (fromGregorian 1900 1 1) (secondsToDiffTime 0)
   in Timestamp . realToFrac $ diffUTCTime utcTime startOfTheCentury

addSecs :: Timestamp -> Double -> Timestamp
addSecs (Timestamp t) secs = Timestamp (t + secs)

diffTimestamps :: Timestamp -> Timestamp -> Double
diffTimestamps (Timestamp t1) (Timestamp t0) = t1 - t0

{-
instance NFData OSCDatum where
   rnf (OSC_I x) = rnf x
   rnf (OSC_F x) = rnf x
   rnf (OSC_S x) = rnf x
{-
   rnf (OSC_I8 x) = rnf x
   rnf (OSC_I16 x) = rnf x
-}
   rnf (OSC_B x) = rnf x
-}

initTreeCommand :: OSC
initTreeCommand = OSC "/g_new" [OSC_I 1, OSC_I 0, OSC_I 0]
