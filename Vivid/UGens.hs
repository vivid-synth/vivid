-- | Unit Generators, which are the signal-generating/processing components of synths.
-- 
--   Most of your time reading documentation will probably be in this module
-- 
--   Most of these take named arguments with types like 'In', 'Freq', etc.
--   This just means you construct them with the same data constructor.
--   The data constructor is the same as its type ('In' and 'In', etc.).
--   So e.g. to make a lowpass filter which filters whitenoise at 440hz, you'd write:
-- 
--   > lpf (In whiteNoise) (Freq 440)
-- 
--   This is far from all the ones in SC, so I've exposed the internals so you can make
--   your own when you want. Some exports may disappear in future versions.

{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE NoRebindableSyntax #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Vivid.UGens (

   -- * Generators
   -- | Generate signals, which can then be processed

   -- ** Audio or control rate
   -- | These can be used as direct sound sources or as control parameters

     lfTri
   , lfSaw
   , sinOsc
   , fSinOsc
   , whiteNoise
   , pinkNoise
   , brownNoise


   -- ** Control rate
   -- | These wouldn't be useful as direct sound sources, but instead as
   --   parameters to other UGens

   , xLine
   , line

   -- * User input
   -- | Generators which get signals from user input

   -- ** Audio rate

   , soundIn0

   -- ** Control rate

   , mouseX
   , mouseY


   -- * Filters
   -- | Filter signals

   , bpf
   , lpf
   , hpf
   , clip


   -- * Buffers

   , playBuf1
   , recordBuf1


   -- * FFT
   -- | Stuff for Fast Fourier Transforms. Very incomplete atm.

   , localBuf
   , fft
   , ifft
   , pv_binScramble
   , pv_randComb


   -- * Signal math
   -- | Add, multiply, etc.

   -- ** Operators
   -- | Mnemonic: the ~ looks like a sound wave

   , (~*)
   , (~+)
   , (~/)
   , (~-)
   , (~>)

   -- ** Functions

   , midiCPS
   , abs'
   , neg
   , binaryOp
   , biOp
   , unaryOp
   , uOp

   -- * Uncategorized
   -- | Haven't organized yet
   , varSaw
   , syncSaw
   , impulse
   , pan2
   , out
   , lfPar
   , lfCub
   , lfPulse
   , mix
   , freeVerb
   , pitchShift
   , lag

   , module Vivid.UGens.Args
   ) where

import Vivid.SynthDef
import Vivid.UGens.Args

import Control.Applicative
import Data.ByteString (ByteString)
import Data.List.Split (chunksOf)

-- | \"A non-band-limited triangle oscillator. Output ranges from -1 to +1.\"
lfTri :: Freq -> SDState Signal
lfTri (Freq freq) = do
   freq' <- toSigM freq
   addUGen $ UGen (UGName_S "LFTri") AR [freq', Constant 0] 1

-- | \"A non-band-limited sawtooth oscillator. Output ranges from -1 to +1.\"
lfSaw :: Freq -> SDState Signal
lfSaw (Freq freq) = do
   freq' <- toSigM freq
   addUGen $ UGen (UGName_S "LFSaw") AR [freq', Constant 0] 1

-- | \"Generates noise whose spectrum has equal power at all frequencies.\"
whiteNoise :: SDState Signal
whiteNoise = addUGen $ UGen (UGName_S "WhiteNoise") AR [] 1

-- | \"Generates noise whose spectrum falls off in power by 3 dB per octave. This gives equal power over the span of each octave. This version gives 8 octaves of pink noise.\"
pinkNoise :: SDState Signal
pinkNoise = addUGen $ UGen (UGName_S "PinkNoise") AR [] 1

-- | \"Generates noise whose spectrum falls off in power by 6 dB per octave.\"
brownNoise :: SDState Signal
brownNoise = addUGen $ UGen (UGName_S "BrownNoise") AR [] 1

-- | Sine wave
sinOsc :: Freq -> SDState Signal
sinOsc (Freq i) = do
   i' <- toSigM i
   addUGen $ UGen (UGName_S "SinOsc") AR [i'] 1

-- | Band-pass filter
bpf :: In -> Freq -> Rq -> SDState Signal
-- Rq: bandwidth / cutofffreq
bpf (In i) (Freq freq) (Rq rq) = do
   i' <- toSigM i
   freq' <- toSigM freq
   rq' <- toSigM rq
   addUGen $ UGen (UGName_S "BPF") AR [i', freq', rq'] 1

-- also look at RLPF:
-- | Low-pass filter
lpf :: In -> Freq -> SDState Signal
lpf = passFilter "LPF"

-- | High-pass filter
hpf :: In -> Freq -> SDState Signal
hpf = passFilter "HPF"

passFilter :: ByteString -> In -> Freq -> SDState Signal
passFilter filterName (In inP) (Freq freq) = do
   in' <- toSigM inP
   freq' <- toSigM freq
   addUGen $ UGen (UGName_S filterName) AR [in', freq'] 1

-- | Unlike in SuperCollider, you don't specify a \"lo\" parameter -- \"lo\" is always
--   negative \"hi\"
clip :: In -> {- Lo -> -} Hi -> SDState Signal
clip (In i) {- (Lo lo) -} (Hi hi) = do
   i' <- toSigM i
   -- lo' <- toSigM lo
   hi' <- toSigM hi
   lo' <- neg hi'
   addUGen $ UGen (UGName_S "Clip") AR [i', lo', hi'] 1

-- | Bus input (usually mic). \"0\" because it's from the 0th bus
soundIn0 :: SDState Signal -- this can easily be expressed now. oh also -- this is a good case where i might want to specify a ton of outputs
soundIn0 = do
   nob <- addUGen $ UGen (UGName_S "NumOutputBuses") IR [] 1
   addUGen $ UGen (UGName_S "In") AR [nob] 1

(~*) :: (ToSigM sig0, ToSigM sig1) => sig0 -> sig1 -> SDState Signal
(~*) = binaryOp Mul

(~+) :: (ToSigM i0, ToSigM i1) => i0 -> i1 -> SDState Signal
(~+) = binaryOp Add

(~/) :: (ToSigM i0, ToSigM i1) => i0 -> i1 -> SDState Signal
(~/) = binaryOp FDiv

(~>) :: (ToSigM i0, ToSigM i1) => i0 -> i1 -> SDState Signal
(~>) = binaryOp Gt

(~-) :: (ToSigM i0, ToSigM i1) => i0 -> i1 -> SDState Signal
(~-) = binaryOp Sub

-- | Build your own!
binaryOp :: (ToSigM s0, ToSigM s1) => BinaryOp -> s0 -> s1 -> SDState Signal
binaryOp theBiOp s0 s1 = do
   s0' <- toSigM s0
   s1' <- toSigM s1
   let sigs = [s0', s1']
   calcRate <- maximum <$> mapM getCalcRate sigs
   addUGen $ UGen (UGName_B theBiOp) calcRate sigs 1

-- | Alias of 'binaryOp'. Shorter, fer livecodin
biOp :: (ToSigM s0, ToSigM s1) => BinaryOp -> s0 -> s1 -> SDState Signal
biOp = binaryOp

-- | Build your own, from 'UnaryOp's
unaryOp :: (ToSigM sig) => UnaryOp -> sig -> SDState Signal
unaryOp theUOp sig = do
   sig' <- toSigM sig
   calcRate <- getCalcRate sig'
   addUGen $ UGen (UGName_U theUOp) calcRate [sig'] 1

-- | Alias of 'unaryOp'
uOp :: (ToSigM sig) => UnaryOp -> sig -> SDState Signal
uOp = unaryOp

-- | Convert from a midi note number (0-127, each representing a musical half step) to a
--   frequency in hz (cycles per second)
midiCPS :: (ToSigM i) => i -> SDState Signal
midiCPS = unaryOp MIDICPS

-- | Inverse of 'midiCPS'
cpsMIDI :: (ToSigM i) => i -> SDState Signal
cpsMIDI = unaryOp CPSMIDI

-- | The prime is to not conflict with \"abs\" in the prelude. May just use
--   \"uOp Abs\" in the future
abs' :: (ToSigM i) => i -> SDState Signal
abs' = unaryOp Abs

neg :: ToSigM i => i -> SDState Signal
neg = unaryOp Neg

out :: (ToSigM i) => Float -> [i] -> SDState [Signal]
out busNum is = do
   is' <- mapM toSigM is
   addPolyUGen $ UGen (UGName_S "Out") AR (Constant busNum : is') (length is)

lfPar :: Freq -> SDState Signal
lfPar (Freq freq) = do
   freq' <- toSigM freq
   addUGen $ UGen (UGName_S "LFPar") AR [freq', Constant 0] 1

-- | \"Generates an exponential curve from the start value to the end value. Both the start and end values must be non-zero and have the same sign.\"
-- 
-- Defaults to KR
xLine :: Start -> End -> Dur -> DoneAction -> SDState Signal
xLine (Start start) (End end) (Dur dur) doneAction = do
   start' <- toSigM start
   end' <- toSigM end
   dur' <- toSigM dur
   addUGen $ UGen (UGName_S "XLine") KR [start', end', dur', Constant $ doneActionNum doneAction] 1

-- | \"Generates a line from the start value to the end value.\"
-- 
-- Defaults to KR
line :: Start -> End -> Dur -> DoneAction -> SDState Signal
line (Start start) (End end) (Dur dur) doneAction = do
   start' <- toSigM start
   end' <- toSigM end
   dur' <- toSigM dur
   addUGen $ UGen (UGName_S "Line") KR [start', end', dur', Constant $ doneActionNum doneAction] 1

lfCub :: Freq -> SDState Signal
lfCub (Freq freq) = do
   freq' <- toSigM freq
   addUGen $ UGen (UGName_S "LFCub") AR [freq'] 1

impulse :: Freq -> SDState Signal
impulse (Freq freq) = do
   freq' <- toSigM freq
   addUGen $ UGen (UGName_S "Impulse") AR [freq', Constant 0] 1

lfPulse :: Freq -> Width -> SDState Signal
lfPulse (Freq freq) (Width width) = do
   freq' <- toSigM freq
   width' <- toSigM width
   addUGen $ UGen (UGName_S "LFPulse") AR [freq', Constant 0, width'] 1


-- other options:
-- warp -- Mapping curve. 0 is linear, 1 is exponential (e. g. for freq or times). Alternatively you can specify: 'linear' or 'exponential'.
-- lag -- Lag factor to dezpipper cursor movement.
mouseY :: MinVal -> MaxVal -> SDState Signal
mouseY = mouseGeneral "MouseY"

mouseX :: MinVal -> MaxVal -> SDState Signal
mouseX = mouseGeneral "MouseX"

mouseGeneral :: ByteString -> (MinVal -> MaxVal -> SDState Signal)
mouseGeneral ugenName (MinVal minVal) (MaxVal maxVal) = do
   minVal' <- toSigM minVal
   maxVal' <- toSigM maxVal
   addUGen $ UGen (UGName_S ugenName) KR [minVal', maxVal', Constant 0, Constant 0.2] 1


varSaw :: Freq -> Width -> SDState Signal
varSaw (Freq freq) (Width width) = do
   freq' <- toSigM freq
   width' <- toSigM width
   addUGen $ UGen (UGName_S "VarSaw") AR [freq', Constant 0, width'] 1

syncSaw :: SyncFreq -> SawFreq -> SDState Signal
syncSaw (SyncFreq syncFreq) (SawFreq sawFreq) = do
   syncFreq' <- toSigM syncFreq
   sawFreq' <- toSigM sawFreq
   addUGen $ UGen (UGName_S "SyncSaw") AR [syncFreq', sawFreq'] 1

-- | Add a single LocalBuf for FFT
localBuf :: NumFrames -> NumChans -> SDState Signal
localBuf (NumFrames numFrames) (NumChans numChannels) = do
   -- don't know what the "1" is here:
   mlb <- addUGen $ UGen (UGName_S "MaxLocalBufs") IR [Constant 1] 1
   numChannels' <- toSigM numChannels
   numFrames' <- toSigM numFrames
   addUGen $ UGen (UGName_S "LocalBuf") IR [numChannels', numFrames', mlb] 1

fft :: Buf -> In -> SDState Signal
fft (Buf buf) (In inp) = do
   buf' <- toSigM buf
   inp' <- toSigM inp
      -- might want to change some of these args:
   let args = [buf', inp', Constant 0.5, Constant 0, Constant 1, Constant 0]
   addUGen $ UGen (UGName_S "FFT") KR args 1

ifft :: Buf -> SDState Signal
ifft (Buf buf) = do
   buf' <- toSigM buf
   addUGen $ UGen (UGName_S "IFFT") AR [buf', Constant 0, Constant 0] 1


   -- FFT FUNCTIONS: --

pv_binScramble :: Buf -> Wipe -> Width -> Trigger -> SDState Signal
pv_binScramble (Buf buf) (Wipe wipe) (Width width) (Trigger trigger) = do
   buf' <- toSigM buf
   wipe' <- toSigM wipe
   width' <- toSigM width
   trigger' <- toSigM trigger
   addUGen $ UGen (UGName_S "PV_BinScramble") KR [buf', wipe', width', trigger'] 1

pv_randComb :: Buf -> Wipe -> Trigger -> SDState Signal
pv_randComb (Buf buf) (Wipe wipe) (Trigger trigger) = do
   buf' <- toSigM buf
   wipe' <- toSigM wipe
   trigger' <- toSigM trigger
   addUGen $ UGen (UGName_S "PV_RandComb") KR [buf', wipe', trigger'] 1



   -- END FFT ---


-- | Mixes down a list of audio rate inputs to one. 
--   The list can't be empty.
-- 
--   This is more efficient than e.g. @foldl1 (~+)@
mix :: (ToSigM s) => [s] -> SDState Signal
mix [] = error "empty mix"
mix [x] = toSigM x
mix xs = mix =<< (mapM mix' . chunksOf 4) =<< mapM toSigM xs
 where
   mix' :: [Signal] -> SDState Signal
   mix' [] = error "something's broken"
   mix' [x] = return x
   mix' [a,b] = a ~+ b
   mix' ins@[_,_,_]   = addUGen $ UGen (UGName_S "Sum3") AR ins 1
   mix' ins@[_,_,_,_] = addUGen $ UGen (UGName_S "Sum4") AR ins 1
   mix' _ = error "that would be weird"

-- can i compute numchans?
-- also e.g. w reverb you dont want the doneaction to be 2
-- | Play a 1-channel buffer
playBuf1 :: {- NumChans -> -} Buf -> SDState Signal
 -- numchans "must be a fixed integer"
   -- args are in sc order, not osc:
playBuf1 {- (NumChans numChans) -} (Buf buf) = do
   -- numChans' <- toSigM numChans
   buf' <- toSigM buf
   addUGen $ UGen (UGName_S "PlayBuf") AR [buf', {- rate: -} Constant 1, {- trigger -} Constant 1, {- startPos -} Constant 0, {- loop: -} Constant 0, {- doneAction -} Constant 2] 1 -- numChans', 

-- | Record a 1-channel buffer
recordBuf1 :: In -> Buf -> SDState Signal
   -- args are in sc order, not osc:
recordBuf1 (In inp) (Buf buf) = do
   in' <- toSigM inp
   buf' <- toSigM buf
   addUGen $ UGen (UGName_S "RecordBuf") AR [buf', {- offset -} Constant 0, {- recLevel -} Constant 1, {- prelevel-} Constant 0, {- run -} Constant 1, {- loop -} Constant 0, {- trigger -} Constant 1, {- doneAction -} Constant 2, in'] 1

freeVerb :: In -> Mix -> Room -> Damp -> SDState Signal
freeVerb (In inp) (Mix mixS) (Room room) (Damp damp) = do
   in' <- toSigM inp
   mix' <- toSigM mixS
   room' <- toSigM room
   damp' <- toSigM damp
   addUGen $ UGen (UGName_S "FreeVerb") AR [in', mix', room', damp'] 1

pitchShift :: In -> Ratio -> SDState Signal
pitchShift (In inp) (Ratio ratio) = do
   in' <- toSigM inp
   ratio' <- toSigM ratio
   addUGen $ UGen (UGName_S "PitchShift") AR [in', {- windowSize: -} Constant 0.2, ratio', {-pitchDispersion -} Constant 0, {- timeDispersion -} Constant 0] 1

fSinOsc :: Freq -> SDState Signal
fSinOsc (Freq freq) = do
   freq' <- toSigM freq
   addUGen $ UGen (UGName_S "FSinOsc") AR [freq'] 1

-- | 'pos' is -1 to 1
pan2 :: In -> Pos -> SDState [Signal]
pan2 (In inp) (Pos pos) = do
   in' <- toSigM inp
   pos' <- toSigM pos
   addPolyUGen $ UGen (UGName_S "Pan2") AR [in', pos'] 2

-- | The \"Secs\" arg is the same as the \"lagTime\" arg in SC
lag :: In -> Secs -> SDState Signal
lag (In inp) (Secs secs) = do
   in' <- toSigM inp
   secs' <- toSigM secs
   addUGen $ UGen (UGName_S "Lag") AR [in', secs'] 1
