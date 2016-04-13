{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs, NoMonoLocalBinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Vivid.SynthDef.ToSig (
     ToSig(..)
   ) where

import Vivid.SynthDef.Types
import Vivid.SCServer.Types

import qualified Data.ByteString.Char8 as BS8 (pack)
import GHC.TypeLits

-- | Don't define other instances of this! (Unless you know what you're doing)
--   Instance resolution could get screwed up.
class ToSig s (args :: [Symbol]) where
   toSig :: s -> SDBody' args Signal

instance ToSig Signal args where
   toSig :: Signal -> SDBody' args Signal
   toSig = return

instance (KnownSymbol a, Subset '[a] args) => ToSig (Variable a) args where
   toSig a = (return . Param . BS8.pack . symbolVal) a

-- Incoherent is just to get numbers defaulting to Floats in a useful way in
-- SynthDefs. The type resolution algorithm should never give weird behavior
-- as long as other instances aren't defined:

-- | For 'Constant' (Float) values
instance {-# INCOHERENT #-} (Num n, Real n) => ToSig n args where
   toSig :: n -> SDBody' args Signal
   toSig = return . Constant . realToFrac

-- This way instead of e.g.
-- > BufferId b <- makeBuffer 1
-- > playBuf (buf_ $ toEnum $ fromEnum b
--
-- we can say:
-- > b <- makeBuffer 1
-- > playBuf (buf_ b
instance ToSig BufferId args where
   toSig :: BufferId -> SDBody' args Signal
   toSig (BufferId n) = (return . Constant . realToFrac) n

instance (a ~ args) => ToSig (SDBody' a Signal) args where
   toSig :: SDBody' args Signal -> SDBody' args Signal
   toSig x = x
