-- | UGen argument labels
-- 
--   These are named the same as their SC counterparts, usually.

{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE NoRebindableSyntax #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies, NoMonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}


{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoUndecidableInstances #-}


module Vivid.UGens.Args where

import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SynthDef
import Vivid.SynthDef.FromUA
-- import Vivid.SynthDef.TypesafeArgs (getSymbolVals)

import qualified Data.ByteString.UTF8 as UTF8 (fromString)
import qualified Data.Map as Map
-- import Data.Monoid
-- import GHC.TypeLits

a_ :: ToSig s as => s -> UA "a" as
a_ = UA . toSig

a0_ :: ToSig s as => s -> UA "a0" as
a0_ = UA . toSig

a1_ :: ToSig s as => s -> UA "a1" as
a1_ = UA . toSig

a2_ :: ToSig s as => s -> UA "a2" as
a2_ = UA . toSig

active_ :: ToSig s as => s -> UA "active" as
active_ = UA . toSig

add_ :: ToSig s as => s -> UA "add" as
add_ = UA . toSig

ampThreshold_ :: ToSig s as => s -> UA "ampThreshold" as
ampThreshold_ = UA . toSig

aReal_ :: ToSig s as => s -> UA "aReal" as
aReal_ = UA . toSig

-- | SC compatibility
areal_ :: ToSig s as => s -> UA "aReal" as
areal_ = aReal_

aImag_ :: ToSig s as => s -> UA "aImag" as
aImag_ = UA . toSig

-- | SC compatibility
aimag_ :: ToSig s as => s -> UA "aImag" as
aimag_ = aImag_

attackSecs_ :: ToSig s as => s -> UA "attackSecs" as
attackSecs_ = UA . toSig

-- | Alias of 'attackSecs_', for SC compatibility
attackTime_ :: ToSig s as => s -> UA "attackSecs" as
attackTime_ = attackSecs_

b_ :: ToSig s as => s -> UA "b" as
b_ = UA . toSig

b1_ :: ToSig s as => s -> UA "b1" as
b1_ = UA . toSig

b2_ :: ToSig s as => s -> UA "b2" as
b2_ = UA . toSig

bias_ :: ToSig s as => s -> UA "bias" as
bias_ = UA . toSig

bins_ :: ToSig s as => s -> UA "bins" as
bins_ = UA . toSig

bits_ :: ToSig s as => s -> UA "bits" as
bits_ = UA . toSig

buf_ :: ToSig s as => s -> UA "buf" as
buf_ = UA . toSig

-- | For SC compatibility -- alias of 'buf_'
buffer_ :: ToSig s as => s -> UA "buf" as
buffer_ = buf_

bus_ :: ToSig s as => s -> UA "bus" as
bus_ = UA . toSig

bw_ :: ToSig s as => s -> UA "bw" as
bw_ = UA . toSig

-- Maybe should actually just be "bw"?:
bwFreq_ :: ToSig s as => s -> UA "bwFreq" as
bwFreq_  = UA . toSig

bwr_ :: ToSig s as => s -> UA "bwr" as
bwr_ = UA . toSig

c_ :: ToSig s as => s -> UA "c" as
c_ = UA . toSig

-- | Alias of 'numChans_'
chans_ :: ToSig s as => s -> UA "numChans" as
chans_ = numChans_

clampSecs_ :: ToSig s as => s -> UA "clampSecs" as
clampSecs_ = UA . toSig

-- | Alias of 'clampSecs_', for SC compatibility
clampTime_ :: ToSig s as => s -> UA "clampSecs" as
clampTime_ = clampSecs_

clar_ :: ToSig s as => s -> UA "clar" as
clar_ = UA . toSig

coef_ :: ToSig s as => s -> UA "coef" as
coef_ = UA . toSig

control_ :: ToSig s as => s -> UA "control" as
control_ = UA . toSig

crossFade_ :: ToSig s as => s -> UA "crossFade" as
crossFade_ = UA . toSig

-- | For SC compatibility -- alias of 'crossFade_'
crossfade_ :: ToSig s as => s -> UA "crossFade" as
crossfade_ = UA . toSig

-- | **This may change in the future**
curve_curve :: Int -> UA "curve" as
curve_curve = UA . return . Constant . realToFrac

{-
curve_step = 0
curve_linear = 1
curve_lin = 1
curve_exponential = 2
curve_exp = 2
curve_sine = 3
curve_sin = 3
curve_welch = 4
curve_wel = 4
curve_squared = 6
curve_sqr = 6
curve_cubed = 7
curve_cub = 7
envCurveNumber (Curve_Curve _) = 5
-}

damp_ :: ToSig s as => s -> UA "damp" as
damp_ = UA . toSig

damping_ :: ToSig s as => s -> UA "damping" as
damping_ = UA . toSig

db_ :: ToSig s as => s -> UA "db" as
db_ = UA . toSig

decaySecs_ :: ToSig s as => s -> UA "decaySecs" as
decaySecs_ = UA . toSig

-- | Alias of 'decaySecs_' for SC compatibility
decayTime_ :: ToSig s as => s -> UA "decaySecs" as
decayTime_ = decaySecs_

-- | Alias of 'decaySecs_' for SC compatibility
decaytime_ :: ToSig s as => s -> UA "decaySecs" as
decaytime_ = decaySecs_

default_ :: ToSig s as => s -> UA "default" as
default_ = UA . toSig

-- | Alias of 'delaySecs_' for SC compatibility
delay_ :: ToSig s as => s -> UA "delaySecs" as
delay_ = delaySecs_

delaySecs_ :: ToSig s as => s -> UA "delaySecs" as
delaySecs_ = UA . toSig

-- | Alias of 'delaySecs_' for SC compatibility
delayTime_ :: ToSig s as => s -> UA "delaySecs" as
delayTime_ = delaySecs_

-- | Alias of 'delaySecs_' for SC compatibility
delaytime_ :: ToSig s as => s -> UA "delaySecs" as
delaytime_ = delaySecs_

density_ :: ToSig s as => s -> UA "density" as
density_ = UA . toSig

depth_ :: ToSig s as => s -> UA "depth" as
depth_ = UA . toSig

depthVariation_ :: ToSig s as => s -> UA "depthVariation" as
depthVariation_ = UA . toSig

div_ :: ToSig s as => s -> UA "div" as
div_ = UA . toSig

dn_ :: ToSig s as => s -> UA "dn" as
dn_ = UA . toSig

doneAction_ :: ToSig s as => s -> UA "doneAction" as
doneAction_ = UA . toSig

downSample_ :: ToSig s as => s -> UA "downSample" as
downSample_ = UA . toSig

dryLevel_, drylevel_ :: ToSig s as => s -> UA "dryLevel" as
dryLevel_ = UA . toSig

drylevel_ = dryLevel_

dsthi_ :: ToSig s as => s -> UA "dsthi" as
dsthi_ = UA . toSig

dstlo_ :: ToSig s as => s -> UA "dstlo" as
dstlo_ = UA . toSig

-- | Alias of 'duration_'
dur_ :: ToSig s as => s -> UA "duration" as
dur_ = duration_

duration_ :: ToSig s as => s -> UA "duration" as
duration_ = UA . toSig

earlyRefLevel_, earlyreflevel_ :: ToSig s as => s -> UA "earlyRefLevel" as
earlyRefLevel_ = UA . toSig

earlyreflevel_ = earlyRefLevel_

end_ :: ToSig s as => s -> UA "end" as
end_ = UA . toSig

execFreq_ :: ToSig s as => s -> UA "execFreq" as
execFreq_ = UA . toSig

-- | Alias of 'exponent_'
exp_ :: ToSig s as => s -> UA "exponent" as
exp_ = exponent_

exponent_ :: ToSig s as => s -> UA "exponent" as
exponent_ = UA . toSig

fftSize_ :: ToSig s as => s -> UA "fftSize" as
fftSize_ = UA . toSig

-- | For SC compatibility -- alias of 'fftSize_'
fftsize_ :: ToSig s as => s -> UA "fftSize" as
fftsize_ = fftSize_

formFreq_ :: ToSig s as => s -> UA "formFreq" as
formFreq_ = UA . toSig

frames_ :: ToSig s as => s -> UA "numFrames" as
frames_ = numFrames_

frameSize_ :: ToSig s as => s -> UA "frameSize" as
frameSize_ = UA . toSig

-- | For SC compatibility -- alias of 'frameSize_'
framesize_ :: ToSig s as => s -> UA "frameSize" as
framesize_ = frameSize_

freeze_ :: ToSig s as => s -> UA "freeze" as
freeze_ = UA . toSig

freq_ :: ToSig s as => s -> UA "freq" as
freq_  = UA . toSig

friction_ :: ToSig s as => s -> UA "friction" as
friction_ = UA . toSig

fundFreq_ :: ToSig s as => s -> UA "fundFreq" as
fundFreq_ = UA . toSig

g_ :: ToSig s as => s -> UA "g" as
g_ = UA . toSig

gain_ :: ToSig s as => s -> UA "gain" as
gain_ = UA . toSig

gate_ :: ToSig s as => s -> UA "gate" as
gate_ = UA . toSig

hi_ :: ToSig s as => s -> UA "hi" as
hi_ = UA . toSig

hop_ :: ToSig s as => s -> UA "hop" as
hop_ = UA . toSig

id_ :: ToSig s as => s -> UA "id" as
id_ = UA . toSig

in_ :: ToSig s as => s -> UA "in" as
in_ = UA . toSig

initFreq_ :: ToSig s as => s -> UA "initFreq" as
initFreq_ = UA . toSig

inputBW_, inputbw_ :: ToSig s as => s -> UA "inputBW" as
inputBW_ = UA . toSig

inputbw_ = inputBW_

integrate_ :: ToSig s as => s -> UA "integrate" as
integrate_ = UA . toSig

-- | Interpolation
interp_ :: ToSig s as => s -> UA "interp" as
interp_ = UA . toSig

-- | For SC compatibility -- alias of 'interp_'
interpolation_ :: ToSig s as => s -> UA "interp" as
interpolation_ = interp_

iphase_ :: ToSig s as => s -> UA "iphase" as
iphase_ = UA . toSig

irBufNum_ :: ToSig s as => s -> UA "irBufNum" as
irBufNum_ = UA . toSig

-- | For SC compatibility -- alias of 'irBufSize_'
irbufnum_ :: ToSig s as => s -> UA "irBufNum" as
irbufnum_ = irBufNum_

kernel_ :: ToSig s as => s -> UA "kernel" as
kernel_ = UA . toSig

-- | Alias, for SC compatibility
lag_ :: ToSig s as => s -> UA "lagSecs" as
lag_ = UA . toSig

lagSecs_ :: ToSig s as => s -> UA "lagSecs" as
lagSecs_ = UA . toSig

-- | For SC compatibility:
lagTime_ :: ToSig s as => s -> UA "lagSecs" as
lagTime_ = lagSecs_

length_ :: ToSig s as => s -> UA "length" as
length_ = UA . toSig

level_ :: ToSig s as => s -> UA "level" as
level_ = UA . toSig

lo_ :: ToSig s as => s -> UA "lo" as
lo_ = UA . toSig

loop_ :: ToSig s as => s -> UA "loop" as
loop_ = UA . toSig

m_ :: ToSig s as => s -> UA "m" as
m_ = UA . toSig

max_ :: ToSig s as => s -> UA "max" as
max_ = UA . toSig

maxBinsPerOctave_ :: ToSig s as => s -> UA "maxBinsPerOctave" as
maxBinsPerOctave_ = UA . toSig

maxDelaySecs_ :: ToSig s as => s -> UA "maxDelaySecs" as
maxDelaySecs_ = UA . toSig

-- | Alias of 'maxDelaySecs_' for SC compatibility
maxDelayTime_ :: ToSig s as => s -> UA "maxDelaySecs" as
maxDelayTime_ = UA . toSig

-- | Alias of 'maxDelaySecs_' for SC compatibility
maxdelaytime_ :: ToSig s as => s -> UA "maxDelaySecs" as
maxdelaytime_ = maxDelayTime_

maxFreq_ :: ToSig s as => s -> UA "maxFreq" as
maxFreq_ = UA . toSig

maxRoomSize_, maxroomsize_ :: ToSig s as => s -> UA "maxRoomSize" as
maxRoomSize_ = UA . toSig

maxroomsize_ = maxRoomSize_

-- | Alias of 'max_', for SC compatibility
maxVal_ :: ToSig s as => s -> UA "max" as
maxVal_ = UA . toSig

median_ :: ToSig s as => s -> UA "median" as
median_ = UA . toSig

min_ :: ToSig s as => s -> UA "min" as
min_ = UA . toSig

minFreq_ :: ToSig s as => s -> UA "minFreq" as
minFreq_ = UA . toSig

minmax_ :: ToSig s as => s -> UA "minmax" as
minmax_ = UA . toSig

-- | Alias of 'min_', for SC compatibility
minVal_ :: ToSig s as => s -> UA "min" as
minVal_ = UA . toSig

mix_ :: ToSig s as => s -> UA "mix" as
mix_ = UA . toSig

mul_ :: ToSig s as => s -> UA "mul" as
mul_ = UA . toSig

numChans_ :: ToSig s as => s -> UA "numChans" as
numChans_ = UA . toSig

numFrames_ :: ToSig s as => s -> UA "numFrames" as
numFrames_ = UA . toSig

numTeeth_ :: ToSig s as => s -> UA "numTeeth" as
numTeeth_ = UA . toSig

offset_ :: ToSig s as => s -> UA "offset" as
offset_ = UA . toSig

onset_ :: ToSig s as => s -> UA "onset" as
onset_ = UA . toSig

peakLevel_ :: ToSig s as => s -> UA "peakLevel" as
peakLevel_ = UA . toSig

peakThreshold_ :: ToSig s as => s -> UA "peakThreshold" as
peakThreshold_ = UA . toSig

phase_ :: ToSig s as => s -> UA "phase" as
phase_ = UA . toSig

pitchDispersion_ :: ToSig s as => s -> UA "pitchDispersion" as
pitchDispersion_ = UA . toSig

pos_ :: ToSig s as => s -> UA "pos" as
pos_ = UA . toSig

post_ :: ToSig s as => s -> UA "post" as
post_ = UA . toSig

preLevel_ :: ToSig s as => s -> UA "preLevel" as
preLevel_ = UA . toSig

rate_ :: ToSig s as => s -> UA "rate" as
rate_ = UA . toSig

rateVariation_ :: ToSig s as => s -> UA "rateVariation" as
rateVariation_ = UA . toSig

radius_ :: ToSig s as => s -> UA "radius" as
radius_ = UA . toSig

ratio_ :: ToSig s as => s -> UA "ratio" as
ratio_ = UA . toSig

recLevel_ :: ToSig s as => s -> UA "recLevel" as
recLevel_ = UA . toSig

relaxSecs_ :: ToSig s as => s -> UA "relaxSecs" as
relaxSecs_ = UA . toSig

-- | Alias of 'relaxSecs_' for SC compatibility
relaxTime_ :: ToSig s as => s -> UA "relaxSecs" as
relaxTime_ = UA . toSig

releaseSecs_ :: ToSig s as => s -> UA "releaseSecs" as
releaseSecs_ = UA . toSig

-- | Alias of 'releaseSecs_', for SC compatibility
releaseTime_ :: ToSig s as => s -> UA "releaseSecs" as
releaseTime_ = releaseSecs_

repeats_ :: ToSig s as => s -> UA "repeats" as
repeats_ = UA . toSig

-- | Shorter alias for 'repeats_'
reps_ :: ToSig s as => s -> UA "repeats" as
reps_ = repeats_

reset_ :: ToSig s as => s -> UA "reset" as
reset_ = UA . toSig

resetPos_ :: ToSig s as => s -> UA "resetPos" as
resetPos_ = UA . toSig

revTime_, revtime_ :: ToSig s as => s -> UA "revTime" as
revTime_ = UA . toSig

-- | Alias, for compatibility
revtime_ = revTime_

room_ :: ToSig s as => s -> UA "room" as
room_ = UA . toSig

roomSize_ :: ToSig s as => s -> UA "roomSize" as
roomSize_ = UA . toSig

-- | Alias, for compatibility
roomsize_ :: ToSig s as => s -> UA "roomSize" as
roomsize_ = roomSize_

root_ :: ToSig s as => s -> UA "root" as
root_ = UA . toSig

rq_ :: ToSig s as => s -> UA "rq" as
rq_ = UA . toSig

rs_ :: ToSig s as => s -> UA "rs" as
rs_ = UA . toSig

run_ :: ToSig s as => s -> UA "run" as
run_ = UA . toSig

sawFreq_ :: ToSig s as => s -> UA "sawFreq" as
sawFreq_ = UA . toSig

secs_ :: ToSig s as => s -> UA "secs" as
secs_ = UA . toSig

shift_ :: ToSig s as => s -> UA "shift" as
shift_ = UA . toSig

slopeAbove_ :: ToSig s as => s -> UA "slopeAbove" as
slopeAbove_ = UA . toSig

slopeBelow_ :: ToSig s as => s -> UA "slopeBelow" as
slopeBelow_ = UA . toSig

spread_ :: ToSig s as => s -> UA "spread" as
spread_ = UA . toSig

spring_ :: ToSig s as => s -> UA "spring" as
spring_ = UA . toSig

srchi_ :: ToSig s as => s -> UA "srchi" as
srchi_ = UA . toSig

srclo_ :: ToSig s as => s -> UA "srclo" as
srclo_ = UA . toSig

startPos_ :: ToSig s as => s -> UA "startPos" as
startPos_ = UA . toSig

start_ :: ToSig s as => s -> UA "start" as
start_ = UA . toSig

step_ :: ToSig s as => s -> UA "step" as
step_ = UA . toSig

stretch_ :: ToSig s as => s -> UA "stretch" as
stretch_ = UA . toSig

susLevel_ :: ToSig s as => s -> UA "susLevel" as
susLevel_ = UA . toSig

syncFreq_ :: ToSig s as => s -> UA "syncFreq" as
syncFreq_ = UA . toSig

tailLevel_, taillevel_ :: ToSig s as => s -> UA "tailLevel" as
tailLevel_ = UA . toSig

taillevel_ = tailLevel_

threshold_ :: ToSig s as => s -> UA "threshold" as
threshold_ = UA . toSig

-- | Alias for "threshold_"
thresh_ :: ToSig s as => s -> UA "threshold" as
thresh_ = threshold_

timeDispersion_ :: ToSig s as => s -> UA "timeDispersion" as
timeDispersion_ = UA . toSig

trig_ :: ToSig s as => s -> UA "trigger" as
trig_ = trigger_

-- | You can use "trig_" instead
trigger_ :: ToSig s as => s -> UA "trigger" as
trigger_ = UA . toSig

trigid_ :: ToSig s as => s -> UA "trigid" as
trigid_ = UA . toSig

-- | Short alias for 'ugen_'
ug_ :: ToSig s as => s -> UA "ugen" as
ug_ = UA . toSig

ugen_ :: ToSig s as => s -> UA "ugen" as
ugen_ = UA . toSig

up_ :: ToSig s as => s -> UA "up" as
up_ = UA . toSig

warp_ :: ToSig s as => s -> UA "warp" as
warp_ = UA . toSig

width_ :: ToSig s as => s -> UA "width" as
width_ = UA . toSig

wipe_ :: (ToSig s as) => s -> UA "wipe" as
wipe_ = UA . toSig

-- | Alias of 'windowSize_'
winsize_ :: ToSig s as => s -> UA "windowSize" as
winsize_ = windowSize_

windowSize_ :: ToSig s as => s -> UA "windowSize" as
windowSize_ = UA . toSig

wintype_ :: ToSig s as => s -> UA "windowType" as
wintype_ = windowType_

windowType_ :: ToSig s as => s -> UA "windowType" as
windowType_ = UA . toSig

xi_ :: ToSig s as => s -> UA "xi" as
xi_ = UA . (toSig)


-- this one gives you:
-- (same as above but "args" at the end there)
--     Could not deduce (FromUA (UA "phase" args0))
-- (this is the same arg as if you have no type sig)


makeMakeUGen :: (
     GetSymbolVals (Vs tags)
   , FromUA optional
   , FromUA userSupplied
   , SDBodyArgs optional ~ SDBodyArgs userSupplied
   , SDBodyArgs optional ~ args
   ) => (UGen -> SDBody' args x) -> Int -> String -> CalculationRate -> Vs tags -> optional -> (userSupplied -> SDBody' args x)
makeMakeUGen addUGenF numOuts sdName calcRate tagList defaultArgs = \userSupplied -> do
   theArgList <- Map.fromList <$> fromUAWithDefaults (DefaultArgs defaultArgs) (OverwritingArgs userSupplied)
   let signals =
          map (\k -> Map.findWithDefault (error $ "that's weird (likely a ugen with a typo in 'Vs'): "++sdName++":"++k) k theArgList) $ getSymbolVals tagList
   addUGenF $ UGen (UGName_S (UTF8.fromString sdName)) calcRate (signals :: [Signal]) numOuts

makeMonoUGen, makeUGen :: (
     GetSymbolVals (Vs tags)
   , FromUA optional
   , FromUA userSupplied
   , SDBodyArgs optional ~ SDBodyArgs userSupplied
   , SDBodyArgs optional ~ args
   ) => String -> CalculationRate -> Vs tags -> optional -> (userSupplied -> SDBody' args Signal)
makeMonoUGen = makeMakeUGen addMonoUGen 1
makeUGen = makeMonoUGen

makePolyUGen :: (
     GetSymbolVals (Vs tags)
   , FromUA optional
   , FromUA userSupplied
   , SDBodyArgs optional ~ SDBodyArgs userSupplied
   , SDBodyArgs optional ~ args
   ) => Int -> String -> CalculationRate -> Vs tags -> optional -> (userSupplied -> SDBody' args [Signal])
makePolyUGen n = makeMakeUGen addPolyUGen n

