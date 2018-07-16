{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, NoMonoLocalBinds #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Conversion (
     a2k
---   , degreeToKey
   , k2a
---   , t2a
---   , t2k
   ) where


import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SynthDef

import qualified Data.ByteString.UTF8 as UTF8


-- | Convert an audio rate signal to a control rate signal
a2k :: ToSig s a => s -> SDBody' a Signal
a2k = AR `to` KR

--- degreeToKey ::
--- degreeToKey =

-- | Convert a control rate signal to an audio rate signal
k2a :: ToSig s a => s -> SDBody' a Signal
k2a = KR `to` AR

--- t2a ::
--- t2a =
--- t2k ::
--- t2k =

to :: ToSig s a => CalculationRate -> CalculationRate -> (s -> SDBody' a Signal)
to fromRate toRate = \s -> do
   s' <- toSig s
   getCalcRate s' >>= \case
      rate | rate == fromRate -> return ()
      _ -> error $ mconcat [
          show fromRate,"->",show toRate
         ," called without a "
         ,show fromRate," input"
         ]
   addUGen $ UGen (UGName_S letters) toRate [s'] 1
 where
   letters = UTF8.fromString [calcLetter fromRate,'2',calcLetter toRate]
   calcLetter = \case
      KR -> 'K'
      AR -> 'A'
      IR -> error "ir?!?!"
      DR -> error "dominican republic?!?!"
