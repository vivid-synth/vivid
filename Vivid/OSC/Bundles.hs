-- | Vivid-specific OSC Bundle encoding

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Vivid.OSC.Bundles (
     encodeOSCBundles
   , initTreeCommand
   ) where

import Vivid.OSC
-- import Vivid.OSC.Old (encodedOSC_addLength)
import qualified Vivid.SC.Server.Commands as SCCmd
import Vivid.SC.Server.Types (NodeId(..))

import Data.ByteString (ByteString)
import qualified Data.List as L
import Data.Monoid

-- TEMP:
import Data.Word
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Binary (encode)
encodedOSC_addLength :: ByteString -> ByteString
encodedOSC_addLength bs =
   BSL.toStrict (encode (toEnum (BS.length bs) :: Word32)) <> bs


-- | Encode OSC bundles, specifically for NRT synthesis.
--   (It's more than just \"mconcat . map 'encodeOSCBundle'\").
-- 
--   Note also that the last action is when the song ends - so if you want
--   e.g. a note to hold at the end you need to add a "wait"
encodeOSCBundles :: [OSCBundle] -> ByteString
encodeOSCBundles bundles =
   mconcat . map (encodedOSC_addLength . encodeOSCBundle) $ withEnd
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



initTreeCommand :: OSC
initTreeCommand =
   SCCmd.g_new (NodeId 1) SCCmd.AddToHead (NodeId 0)

