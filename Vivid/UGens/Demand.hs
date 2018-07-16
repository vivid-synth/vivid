{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.UGens.Demand (
     inf

   , dbrown
     -- In Vivid.UGens.Buffer:
   -- , dbufrd
   -- , dbufwr
   , demand
---   , demandEnvGen
---   , dgeom
   , dibrown
   , diwhite
---   , dpoll
   , drand
---   , dreset
   , dseq
   , dser
---   , dseries
   , dshuf
---   , dstutter
---   , dswitch
---   , dswitch1
---   , duty
   , dwhite
---   , dwrand
   , dxrand
---   , tDuty
   ) where

import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.UGens.Args
import Vivid.SynthDef
-- import Vivid.SynthDef.TypesafeArgs
import Vivid.SynthDef.FromUA

-- import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import Data.Proxy

-- | This correctly decodes/encodes to OSC:
inf :: Float
inf = 1/0

-- | "Dbrown returns numbers in the continuous range between lo and hi , Dibrown returns integer values."
-- 
--   "The arguments can be a number or any other UGen."
-- 
--   "See Pbrown, BrownNoise for structurally related equivalents."
dbrown :: Args '[] '["lo","hi","step","length"] a => a -> SDBody a Signal
dbrown = demandBrownian "Dbrown"

-- | Defaults to 'KR'
demand :: Args '["trigger","reset","ugen"] '[] a => a -> SDBody a Signal
demand = makeUGen
   "Demand" KR
   (Vs::Vs '["trigger","reset","ugen"])
   NoDefaults

--- demandEnvGen ::
--- demandEnvGen =
--- dgeom ::
--- dgeom =

dibrown :: Args '[] '["lo","hi","step","length"] a => a -> SDBody a Signal
dibrown = demandBrownian "Dibrown"

demandBrownian :: String -> (Args '[] '["lo","hi","step","length"] a => a -> SDBody a Signal)
demandBrownian ugName = makeUGen
   ugName DR
    -- another example of SC args out of order:
   (Vs::Vs '["length","lo","hi","step"])
   (lo_ (0::Float), hi_ (1::Float), step_ (0.01::Float), length_ inf)

diwhite :: Args '[] '["lo","hi","length"] a => a -> SDBody a Signal
diwhite = demandWhite "Diwhite"

--- dpoll ::
--- dpoll =

-- | \"'dxrand' never plays the same value twice, whereas drand chooses any value in the list\"
drand :: (Args '[] '["repeats"] a, ToSig s (SDBodyArgs a)) => a -> [s] -> SDBody a Signal
drand = drawFromList "Drand"

--- dreset ::
--- dreset =

-- | The list come second so you can curry the repeats and use '=<<' or '>>='
dseq :: (Args '[] '["repeats"] a, ToSig s (SDBodyArgs a)) => a -> [s] -> SDBody a Signal
dseq = drawFromList "Dseq"


drawFromList :: (Args '[] '["repeats"] a, ToSig s (SDBodyArgs a)) => String -> a -> [s] -> SDBody a Signal
drawFromList ugName args sigs = do
   sigs' <- mapM toSig sigs
   reps <- uaArgValWDefault (1::Float) args (Proxy::Proxy "repeats")
   addUGen $ UGen (UGName_S $ UTF8.fromString ugName) DR (reps:sigs') 1

dser :: (Args '[] '["repeats"] a, ToSig s (SDBodyArgs a)) => a -> [s] -> SDBody a Signal
dser = drawFromList "Dser"


--- dseries ::
--- dseries =

dshuf :: (Args '[] '["repeats"] a, ToSig s (SDBodyArgs a)) => a -> [s] -> SDBody a Signal
dshuf = drawFromList "Dshuf"

--- dstutter ::
--- dstutter =
--- dswitch ::
--- dswitch =
--- dswitch1 ::
--- dswitch1 =
--- duty ::
--- duty =

dwhite :: Args '[] '["lo","hi","length"] a => a -> SDBody a Signal
dwhite = demandWhite "Dwhite"

demandWhite :: String -> (Args '[] '["lo","hi","length"] a => a -> SDBody a Signal)
demandWhite ugName = makeUGen
   ugName DR
    -- another example of SC args out of order:
   (Vs::Vs '["length","lo","hi"])
   (lo_ (0::Float), hi_ (1::Float), length_ inf)

--- dwrand ::
--- dwrand =

-- | \"dxrand never plays the same value twice, whereas 'drand' chooses any value in the list\"
dxrand :: (Args '[] '["repeats"] a, ToSig s (SDBodyArgs a)) => a -> [s] -> SDBody a Signal
dxrand = drawFromList "Dxrand"

--- tDuty ::
--- tDuty =
