{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE NoRebindableSyntax #-}

module Vivid.OSC.Util (
     align
   , floatToWord
   , wordToFloat
   ) where

import Data.Bits ((.&.), complement, Bits)
import qualified Foreign as F
import System.IO.Unsafe (unsafePerformIO)

-- from hosc:
align :: (Num i,Bits i) => i -> i
{-# INLINE align #-}
align n = ((n + 3) .&. complement 3) - n


-- from data-binary-ieee754:
floatToWord :: Float -> F.Word32
floatToWord = coercionThing

wordToFloat :: F.Word32 -> Float
wordToFloat = coercionThing

coercionThing :: (F.Storable a, F.Storable b) => a -> b
coercionThing x = unsafePerformIO $ F.alloca $ \buf -> do
   F.poke (F.castPtr buf) x
   F.peek buf
