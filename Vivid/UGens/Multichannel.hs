{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Multichannel (

     -- * Multichannel > Ambisonics

---     biPanB2
---   , decodeB2
---   , panB
---   , panB2
---   , rotate2

     -- * Multichannel > Panners

---   , balance2
---   , linPan2
     pan2
---   , pan4
---   , panAz
     -- See above:
   -- , rotate2
   , splay
---   , splayAz

     -- * Multichannel > Select

---   , linSelectX
---   , linXFade2
   , select
---   , selectX
---   , selectXFocus
---   , xFade2

     -- * Multichannel
     
   , mix
---   , numChannels

   , addChannels
   ) where

import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SynthDef
--import Vivid.SynthDef.FromUA
import Vivid.UGens.Algebraic
import Vivid.UGens.Args
import Vivid.UGens.Generators.SingleValue (dc)
import Vivid.SynthDef.FromUA

import Control.Monad (foldM, zipWithM)
import Data.List.Split (chunksOf)

--- biPanB2 ::
--- biPanB2 =
--- decodeB2 ::
--- decodeB2 =
--- panB ::
--- panB =
--- panB2 ::
--- panB2 =
--- rotate2 ::
--- rotate2 =
--- balance2 ::
--- balance2 =
--- linPan2 ::
--- linPan2 =

-- | 'pos' is -1 to 1
-- 
--   'level' is \"a control-rate level input\"
pan2 :: Args '["in","pos"] '["level"] a => a -> SDBody a [Signal]
pan2 = makePolyUGen
   2 "Pan2" AR
   (Vs::Vs '["in","pos","level"])
   (level_ (1::Float))

-- return a tuple?: -- no no that's exactly when you run into problems with the foldable shit -- people get a tuple when they expect a list....
--- pan4 ::
--- pan4 =
--- panAz ::
--- panAz =

-- | "Spreads [a list] of channels across the stereo field."
splay :: ToSig s a => [s] -> SDBody' a [Signal]
splay sigsMono = do
   -- sigs' <- mapM toSig sigs
   let numChans :: Float
       numChans = toEnum $ max 2 $ (length::[a]->Int) sigsMono
   let positions :: [Float]
       positions =
         map ((\x->x-1) . (*(2/(numChans-1)))) [0..(numChans-1)]
    -- note SC has a different calculation for KR:
   let level = sqrt (recip numChans)
   sigsStereo <- (\x -> zipWithM x sigsMono positions) $ \sig pos ->
      pan2 (in_ sig, pos_ pos)
   -- todo: is the rate correct in ALL cases?:
   mapM (level ~*) =<< foldM addChannels [] sigsStereo


-- | "Spreads [a list] of channels across the stereo field. Optional arguments are spread
--   and center, and equal power levelCompensation. The formula for the stereo position
--   is:
-- 
--   > ((0 .. (n - 1)) * (2 / (n - 1) - 1) * spread + center
--- splay' :: 
--- splay' =

--- splayAz ::
--- splayAz =

-- Don't implement: the geometry is wrong here and it's been deprecated:
-- splayZ ::

--- linSelectX ::
--- linSelectX =
--- linXFade2 ::
--- linXFade2 =

select :: ToSig s as => s -> [SDBody' as Signal] -> SDBody' as Signal
select which array = do
   which' <- toSig which
   array' <- mapM toSig array
   addUGen $ UGen (UGName_S "Select") AR (which' : array') 1

--- selectX ::
--- selectX =
--- selectXFocus ::
--- selectXFocus =
--- xFade2 ::
--- xFade2 =

-- | Mixes down a list of audio rate inputs to one. 
-- 
--   This is more efficient than e.g. @foldl1 (~+)@
--
--   If the list is empty this is the same as @dc 0@
mix :: ToSig s a => [s] -> SDBody' a Signal
mix [] = dc 0
mix [x] = toSig x
mix xs = mix =<< (mapM mix' . chunksOf 4) =<< mapM toSig xs
 where
   mix' :: [Signal] -> SDBody' a Signal
   mix' [] = error "something's broken"
   mix' [x] = return x
   mix' [a,b] = a ~+ b
   mix' ins@[_,_,_]   = addUGen $ UGen (UGName_S "Sum3") AR ins 1
   mix' ins@[_,_,_,_] = addUGen $ UGen (UGName_S "Sum4") AR ins 1
   mix' _ = error "that would be weird"

--- numChannels ::
--- numChannels =

-- | Like 'zipWithM' but if the lists are of different lengths, doesn't shorten the longer one
addChannels :: (ToSig s0 a, ToSig s1 a) => [s0] -> [s1] -> SDBody' a [Signal]
addChannels [] xs = mapM toSig xs
addChannels xs [] = mapM toSig xs
addChannels (x:xs) (y:ys) = do
   foo <- toSig x ~+ toSig y
   (foo:) <$> addChannels xs ys
