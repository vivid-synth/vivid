-- | Actions. 'Vivid.Actions.Class.VividAction' has 3 instances:
-- 
--   - "Vivid.Actions.IO" : happens right here, right now
--   - "Vivid.Actions.Scheduled" : happens at some point in the (maybe near) future.
--        The timing is precise, unlike IO
--   - "Vivid.Actions.NRT" : non-realtime. Writes to an audio file

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

-- For 'MonoOrPoly':
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Vivid.Actions (
     synth
   , synthG
   , synthNamed
   , synthNamedG
   , set
   , play
   , free
   , freeBuf
   , quitSCServer
   , module Vivid.Actions.Class
   -- , module Vivid.Actions.IO
   , module Vivid.Actions.NRT
   , module Vivid.Actions.Scheduled

   , makeSynth
   , synthWAction
   ) where

import Vivid.Actions.Class
import Vivid.Actions.IO ()
import Vivid.Actions.NRT
import Vivid.Actions.Scheduled
import Vivid.OSC
import Vivid.SCServer.Connection (closeSCServerConnection)
import Vivid.SCServer.Types (NodeId(..), Node(..), HasNodeId(..), BufferId(..))
import Vivid.SynthDef (getSDHashName, sd {- , SDBody -}) -- SynthDef(..), SDBody, Signal)
-- import Vivid.SynthDef.Types (SDName(..))
import Vivid.SynthDef.TypesafeArgs

import Control.Arrow (first) -- , second)
import qualified Data.ByteString.Char8 as BS8 (pack, ByteString)
import Data.Int
import Data.Monoid

-- for "play":
import qualified Data.Map as Map
-- import Data.Map (Map)

-- for "play":
import Vivid.SynthDef (makeSynthDef)
-- import Vivid.SynthDef.FromUA (NoDefaults(..))
import Vivid.SynthDef.Types
import Vivid.UGens.InOut (out)

-- | Given a UGen graph, just start playing it right away.
-- 
--   e.g.
-- 
--   > play $ do
--   >    s <- 0.2 ~* lpf (in_ whiteNoise, freq_ 440)
--   >    out 0 [s, s]
-- 
--   The "out" is optional, too -- so you can write
-- 
--   > play $ 0.2 ~* lpf (in_ whiteNoise, freq_ 440)
-- 
--   and an "out" will be added, in stereo
play :: (VividAction m, MonoOrPoly s) => SDBody' '[] s -> m (Node '[])
play monoOrPolyVersion = do
   let polyVersion = getPoly monoOrPolyVersion
   let sdUGens :: [(UGenName, CalculationRate)]
       sdUGens =
          map (\x -> (_ugenName x, _ugenCalculationRate x)) $
             Map.elems $ _sdUGens $ makeSynthDef SDName_Hash () polyVersion
       -- Note this doesn't check that it's the last one that's
       --   the "Out":
   let sdWithOut = sd () $
          if any (==(UGName_S "Out", AR)) sdUGens
             then polyVersion
             else do
                foo <- polyVersion
                out (0::Int) $ foo
   synth sdWithOut ()

class MonoOrPoly s where
   getPoly :: SDBody' a s -> SDBody' a [Signal]

instance MonoOrPoly [Signal] where
   getPoly :: SDBody' a [Signal] -> SDBody' a [Signal]
   getPoly = id

instance MonoOrPoly Signal where
   getPoly :: SDBody' a Signal -> SDBody' a [Signal]
   -- getPoly s = (:[]) <$> s
   -- should this be stereo? a lot of uses call for something else
   getPoly s = do
      s' <- s
      return [s', s']

-- | Immediately stop a synth playing
-- 
--   This can create a \"clipping\" artifact if the sound goes from a high
--   amplitude to 0 in an instant -- you can avoid that with e.g.
--   'Vivid.UGens.lag' or with an envelope (especially 'envGate')
free :: (VividAction m, HasNodeId n) => n -> m ()
free (getNodeId -> NodeId nodeId) =
   callOSC $ OSC (BS8.pack "/n_free") [ OSC_I nodeId ]

-- | Set the given parameters of a running synth
-- 
--   e.g.
-- 
--   >>> let setTest = sd (0.05 ::I "pan") $ out 0 =<< pan2 (in_ $ 0.1 ~* whiteNoise, pos_ (A::A "pan"))
--   >>> s <- synth setTest ()
--   >>> set s (-0.05 ::I "pan")
-- 
--   Any parameters not referred to will be unaffected, and any you specify that don't exist
--   will be (silently) ignored
set :: (VividAction m, Subset (InnerVars params) sdArgs, VarList params) => Node sdArgs -> params -> m ()
set (Node (NodeId nodeId)) params = do
   let (as, _) = makeTypedVarList params
   callOSC $ OSC (BS8.pack "/n_set") $ OSC_I nodeId : paramList as
 where
   paramList :: [(String, Float)] -> [OSCDatum]
   paramList ps = concatMap (\(k,v)->[OSC_S k,OSC_F v]) $
      map (first BS8.pack) ps

-- | Create a real live music-playing synth from a boring, dead SynthDef.
-- 
--   If you haven't defined the SynthDef on the server, this will do it automatically
--   (Note that this may cause jitters in musical timing)
-- 
--   Given...
-- 
--   >>> let foo = sd () $ out 0 [0.1 ~* whiteNoise]
-- 
--   ...you can create a synth with...
-- 
--   >>> synth foo ()
-- 
--   Careful!: The SC server doesn't keep track of your nodes for you,
--   so if you do something like...
-- 
--   >>> s <- synth someSynth ()
--   >>> s <- synth oops ()           -- 's' is overwritten
-- 
--   ...you've got no way to refer to the first synth you've created, and if you
--   want to stop it you have to 'cmdPeriod'
-- 
--   (If you want to interop with SC's language, use 'sdNamed' and 'synthNamed')
synth :: (VividAction m, VarList params, Subset (InnerVars params) args) => SynthDef args -> params -> m (Node args)
synth theSD params = do
   Node <$> synthG theSD params

synthWAction :: (VividAction m, VarList params, Subset (InnerVars params) args) => SynthDef args -> params -> Int32 -> m (Node args)
synthWAction theSD params actionNum = do
   Node <$> synthG_wAction theSD params actionNum

-- | Make a synth, "G"radually typed -- doesn't check that _ is a subset of _
--   Useful e.g. if you want to send a bunch of args, some of which may be discarded
-- 
--   (Personally I'd recommend not using this function)
-- 
--   >>>  let s = undefined :: SynthDef '["ok"]
--   >>>  synth s (4::I "ok", 5::I "throwaway")
--   >>>     <interactive>:275:7:
--   >>>         Could not deduce (Elem "ignore" '[]) arising from a use of ‘synth’
--   >>>  synthG s (4::I "ok", 5::I "throwaway")
--   >>>   (works)
synthG :: (VividAction m, VarList params) => SynthDef a -> params -> m NodeId
synthG theSD params = do
   defineSD theSD -- 'defineSD' only defines it if it hasn't been yet

   let synthName = case theSD of
        SynthDef (SDName_Named n) _ _ -> n
        SynthDef SDName_Hash _ _ -> getSDHashName theSD
   makeSynth synthName params 0


synthG_wAction :: (VividAction m, VarList params) => SynthDef a -> params -> Int32 -> m NodeId
synthG_wAction theSD params actionNum = do
   defineSD theSD -- 'defineSD' only defines it if it hasn't been yet

   let synthName = case theSD of
        SynthDef (SDName_Named n) _ _ -> n
        SynthDef SDName_Hash _ _ -> getSDHashName theSD
   makeSynth synthName params actionNum


synthNamed :: (VividAction m, VarList params) => String -> params -> m (Node a)
synthNamed name params = Node <$> makeSynth (BS8.pack name) params 0

synthNamedG :: (VividAction m, VarList params) => String -> params -> m NodeId
synthNamedG name params = makeSynth (BS8.pack name) params 0

-- | addAction options, from SC docs:
-- 
--   - 0: add the new node to the the head of the group specified by the add target ID.
--   - 1: add the new node to the the tail of the group specified by the add target ID.
--   - 2: add the new node just before the node specified by the add target ID.
--   - 3: add the new node just after the node specified by the add target ID.
--   - 4: the new node replaces the node specified by the add target ID. The target node is freed.
makeSynth :: (VividAction m, VarList params) => BS8.ByteString -> params -> Int32 -> m NodeId
makeSynth synthName params addAction = do
   nodeId@(NodeId nn) <- newNodeId
   callOSC $ OSC (BS8.pack "/s_new") $ [
        OSC_S $ synthName
      , OSC_I nn
      , OSC_I addAction
       -- the target of the add action:
      , OSC_I 1
      ] <> paramList

   return nodeId
 where
   paramList :: [OSCDatum]
   paramList = concatMap (\(k, v) -> [OSC_S k, OSC_F v]) $
      map (first BS8.pack) (fst $ makeTypedVarList params)

-- | Stop the SuperCollider server
quitSCServer :: IO ()
quitSCServer = do
   callOSC $ OSC "/quit" []
   closeSCServerConnection

freeBuf :: VividAction m => BufferId -> m ()
freeBuf (BufferId bufId) =
   callOSC $ OSC "/b_free" [OSC_I bufId]
