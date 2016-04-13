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
---   , splay
---   , splayAz
---   , splayZ

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
   ) where

import Vivid.SynthDef
--import Vivid.SynthDef.FromUA
import Vivid.UGens.Algebraic
import Vivid.UGens.Args
import Vivid.SynthDef.FromUA

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

--- pan4 ::
--- pan4 =
--- panAz ::
--- panAz =
--- splay ::
--- splay =
--- splayAz ::
--- splayAz =
--- splayZ ::
--- splayZ =
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
--   The list can't be empty.
-- 
--   This is more efficient than e.g. @foldl1 (~+)@
mix :: ToSig s a => [s] -> SDBody' a Signal
mix [] = error "empty mix"
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
