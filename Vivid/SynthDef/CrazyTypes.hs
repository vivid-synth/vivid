-- | Our IncoherentInstances + UndecidableInstances sin bin, for everything that needs
--   crazy type hacks
-- 
--   Most of this is just to get numbers defaulting to Floats in a useful way in
--   SynthDefs
-- 
--   We keep these separated so everything that doesn't need IncoherentInstances
--   can live in Sanity Land

{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module Vivid.SynthDef.CrazyTypes where

-- import Vivid.SynthDef ()
import Vivid.SynthDef.Types

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8



class ToSig s where
  toSig :: s -> Signal

instance ToSig Signal where
   toSig = id

-- | For 'Constant' (Float) values
instance (Num a, Real a) => ToSig a where
   toSig = Constant . fromRational . toRational

instance ToSig String where
   toSig = Param . BS8.pack

---

class ToSigM s where
   toSigM :: s -> SDState Signal

instance (ToSig i) => ToSigM i where
   toSigM = return . toSig

instance ToSigM (SDState Signal) where
   toSigM = id

---

class HasSynthRef a where
   getSynthRef :: a -> Either ByteString SynthDef

-- for some reason this needs -XFlexibleInstances:
instance HasSynthRef String where
   getSynthRef = Left . BS8.pack

instance HasSynthRef SynthDef where
   getSynthRef = Right

-- can also do:
{-
instance HasSynthRef (SDState Input) where
   getSynthRef = Right . sd []
-}
