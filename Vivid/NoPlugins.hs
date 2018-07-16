-- | Exports everything from Vivid (and some helpful reexports) except 'Vivid.UGens.Plugins'

{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE DataKinds #-}

{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.NoPlugins (
   -- * Vivid Reexports
     module Vivid.Actions
   , module Vivid.Envelopes
   , module Vivid.SynthDef
   , module Vivid.UGens

   , module Vivid.SCServer
   , module Vivid.Randomness
   , addSecs
   , Timestamp(..)

   , module Vivid.SynthDef.TypesafeArgs
   , module Vivid.SynthDef.FromUA

   -- * Vivid-supercollider Reexports:
   , module Vivid.SC.SynthDef.Types

   -- * Handy Reexports For Livecoding
   --   So you need to spend as little time importing as possible while livecoding
   , module Control.Applicative
   , module Control.Monad
   , module Data.ByteString
   , module Data.Int
   , (&)
   , module Data.Monoid
   , module System.Random
   , module Control.Monad.Random
   , module Control.Monad.IO.Class


   , bufToI
   , bToI
   , b2i
   ) where

-- Reexports from vivid-supercollider:
import Vivid.SC.SynthDef.Types

import Vivid.Actions
import Vivid.Envelopes
import Vivid.SCServer
import Vivid.Randomness
import Vivid.SynthDef
import Vivid.SynthDef.FromUA (Args, SDBodyArgs, UA, NoDefaults, none) -- To make type sigs not have qualified names
import Vivid.SynthDef.TypesafeArgs (AddParams)

import Vivid.UGens
import Vivid.OSC (addSecs)

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Int
import Data.Monoid
import System.Random

import Control.Monad.IO.Class
import GHC.TypeLits



bufToI :: KnownSymbol a => BufferId -> I a
bufToI (BufferId b) = toI b

bToI :: KnownSymbol a => BufferId -> I a
bToI = bufToI

b2i :: KnownSymbol a => BufferId -> I a
b2i = bufToI
