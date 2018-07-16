-- | Not exported by default because many of these have the same
--   names as in "Control.Arrow"

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Vivid.ByteBeat (
     (&&&)
   , (|||)
   , (>>>)
   , (<<<)
     -- Reexported from Vivid.UGens.Algebraic:
   , xor

   , byteBeatFromHelpFile
   , baseThing
   ) where


import Vivid.SynthDef
-- import Vivid.SynthDef.ToSig
import Vivid.UGens.Algebraic
import Vivid.UGens.Args
import Vivid.UGens.Generators.Deterministic (impulse)
import Vivid.UGens.Triggers (pulseCount)

(&&&), (|||), (>>>), (<<<)
   :: (ToSig s0 a, ToSig s1 a) => s0 -> s1 -> SDBody' a Signal
-- | Bitwise @and@
(&&&) = biOp BitAnd
-- | Bitwise @or@
(|||) = biOp BitOr
-- | Bit shift right
(>>>) = biOp ShiftRight
-- | Bit shift left
(<<<) = biOp ShiftLeft
-- Also 'xor' from Vivid.UGens.Algebraic


-- | E.g.
-- 
--   > play byteBeatFromHelpFile
byteBeatFromHelpFile :: SDBody' a Signal
byteBeatFromHelpFile = baseThing $ \t ->
   (((t ~* 15) &&& (t >>> 5)) |||
    ((t ~* 5)  &&& (t >>> 3)) |||
    ((t ~* 2)  &&& (t >>> 9)) |||
    ((t ~* 8)  &&& (t >>> 11)))
   ~- 3

-- | E.g.
-- 
--   > play $ baseThing $ \t -> t &&& (t >>> 8)
baseThing :: (Signal -> SDBody' a Signal) -> SDBody' a Signal
baseThing f = do
   t <- pulseCount (trig_ $ impulse (freq_ 8e3))
   sig0 <- f t
   sig1 <- biOp Mod sig0 256
   ((sig1 ~/ 127) ~- 1) ~* 0.1
