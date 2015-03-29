-- | These are named the same as their SC counterparts, usually.
--   Sometimes not, if the names are long or if the names would clash w/ common
--   other names.

{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE NoRebindableSyntax #-}

{-# LANGUAGE ExistentialQuantification #-}

module Vivid.UGens.Args where

import Vivid.SynthDef (ToSigM)

data Buf = forall i. ToSigM i => Buf i
data Bus = forall i. ToSigM i => Bus i
data Damp = forall i. ToSigM i => Damp i
data Dur = forall i. ToSigM i => Dur i
data End = forall i. ToSigM i => End i
data Freq = forall i. ToSigM i => Freq i
data Hi = forall i. ToSigM i => Hi i
data In = forall i. ToSigM i => In i
data Lo = forall i. ToSigM i => Lo i
data MaxVal = forall i. ToSigM i => MaxVal i
data MinVal = forall i. ToSigM i => MinVal i
data Mix = forall i. ToSigM i => Mix i
data NumChans = forall i. ToSigM i => NumChans i
data NumFrames = forall i. ToSigM i => NumFrames i
data Phase = forall i. ToSigM i => Phase i
data Pos = forall i. ToSigM i => Pos i
data Ratio = forall i. ToSigM i => Ratio i
data Room = forall i. ToSigM i => Room i
data Rq = forall i. ToSigM i => Rq i
data SawFreq = forall i. ToSigM i => SawFreq i
data Secs = forall s. ToSigM s => Secs s
data Start = forall i. ToSigM i => Start i
data SyncFreq = forall i. ToSigM i => SyncFreq i
data Trigger = forall i. ToSigM i => Trigger i
data Width = forall i. ToSigM i => Width i
data Wipe = forall i. ToSigM i => Wipe i
