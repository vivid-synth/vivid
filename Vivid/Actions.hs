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
   , newSynthBefore
   , synthBefore
   , newSynthAfter
   , synthAfter
   , newSynthAtHead
   , synthHead
   , synthOn
   , newSynthAtTail
   , synthTail

   , newGroup
   , newGroupBefore
   , newGroupAfter
   , newGroupAtHead
   , newGroupAtTail
   , newParGroup
   , newParGroupBefore
   , newParGroupAfter
   , newParGroupAtHead
   , newParGroupAtTail

   , set
   , play
   , free
   , freeSynth
   , release
   , releaseIn
   , freeBuf
   , quitSCServer
   , module Vivid.Actions.Class
   -- , module Vivid.Actions.IO
   , module Vivid.Actions.NRT
   , module Vivid.Actions.Scheduled

   , makeSynth
   ) where

import Vivid.OSC
import qualified Vivid.SC.Server.Commands as SCCmd
import Vivid.SC.Server.Types
import Vivid.SC.SynthDef.Types (CalculationRate(..))
import Vivid.SC.Server.Types

import Vivid.Actions.Class
import Vivid.Actions.IO ()
import Vivid.Actions.NRT
import Vivid.Actions.Scheduled
import Vivid.SCServer.Connection (closeSCServerConnection)
import Vivid.SCServer (defaultGroup)
import Vivid.SCServer.Types
import Vivid.SynthDef (getSDHashName, sd {- , SDBody -}) -- SynthDef(..), SDBody, Signal)
-- import Vivid.SynthDef.Types (SDName(..))
import Vivid.SynthDef.TypesafeArgs

import Control.Arrow (first) -- , second)
import qualified Data.ByteString.UTF8 as UTF8
import Data.ByteString (ByteString)
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

-- todo: this segfaults if you have a kr output i think:
-- e.g.:
   -- let foo = kOut_mono 100 =<< (phasor (end_ 999) ? KR)
   -- play $ foo
   -- it has the same output as sclang:
   -- putStrLn $ sdLitPretty $ sdToLiteral $ sd () $ foo >> out 0 []
   -- same as:
   -- synthdefcatcher: SynthDef(\a, {Out.kr(100, Phasor.kr(end: 999))}).send(~hsServ)
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
play :: (VividAction m, MonoOrPoly s) => SDBody' '[] s -> m (Synth '[])
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
   -- TODO: maybe 'defineSD'/'sync' first?:
   -- Or maybe that's the job of 'synth' so do it there
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

-- | 'G'radually-typed version of 'freeSynth' or 'free'. Note that this
--   allows you to attempt to free a NodeId of a Group as if it were a
--   synth. Be careful!
freeG :: (VividAction m, SynthOrNodeId n) => n -> m ()
freeG (getNodeId -> nodeId) =
   callOSC $ SCCmd.n_free nodeId

free, freeSynth :: VividAction m => Synth a -> m ()
-- | Shorter name for 'freeSynth'
free = freeSynth
-- | Immediately stop a synth playing
-- 
--   This can create a \"clipping\" artifact if the sound goes from a high
--   amplitude to 0 in an instant -- you can avoid that with e.g.
--   'Vivid.UGens.lag' or with an envelope (especially 'envGate')
freeSynth = freeG

-- | Assuming your \"gate\" argument is on an EnvGen or similar, will release the synth
--   over the EnvGen-specified fade time
-- 
--   If you'd like to specify a fade time in the moment, check out 'releaseIn'
release :: (Elem "gate" args, VividAction m) => Synth args -> m ()
release s = set s (0 ::I "gate")

-- | Assumes your \"gate\" is on an EnvGen or related
-- 
--   Specify a fade time and release
releaseIn :: (Elem "gate" args, VividAction m, Real n) => n -> Synth args -> m ()
releaseIn releaseSecs s =
     -- 'abs' but give positive values yo!:
   set s (toI $ negate $ 1 + abs releaseSecs ::I "gate")

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
set :: (VividAction m, Subset (InnerVars params) sdArgs, VarList params) => Synth sdArgs -> params -> m ()
set (Synth nodeId) params = do
   let (as, _) = makeTypedVarList params
   -- HERE:
   callOSC $ SCCmd.n_set nodeId [ (k, Right v) | (k, v) <- as ]

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
--   (If you want to interop with SC's language (or other SC clients),
--   use 'sdNamed' and 'synthNamed')
synth :: (VividAction m, VarList params, Subset (InnerVars params) args) => SynthDef args -> params -> m (Synth args)
synth theSD params = do
   Synth <$> synthG theSD params

-- | Make a synth, \"G\"radually typed -- doesn't check that _ is a subset of _
--   Useful e.g. if you want to send a bunch of args, some of which may be discarded
-- 
--   (Personally I'd recommend not using this function)
-- 
--   >>>  let s = undefined :: SynthDef '["ok"]
--   >>>  synth s (4::I "ok", 5::I "throwaway")
--   >>>     <interactive>:
--   >>>         Could not deduce (Elem "ignore" '[]) arising from a use of ‘synth’
--   >>>  synthG s (4::I "ok", 5::I "throwaway")
--   >>>   (works)
synthG :: (VividAction m, VarList params) => SynthDef a -> params -> m NodeId
synthG theSD params = do
   defineSD theSD -- 'defineSD' only defines it if it hasn't been yet

   makeSynth (synthName theSD) params SCCmd.AddToHead defaultGroup

synthName :: SynthDef a -> ByteString
synthName theSD = case theSD of
        SynthDef (SDName_Named n) _ _ -> n
        SynthDef SDName_Hash _ _ -> getSDHashName theSD

synthNamed :: (VividAction m, VarList params) => String -> params -> m (Synth a)
synthNamed name params = Synth <$>
   makeSynth (UTF8.fromString name) params SCCmd.AddToHead defaultGroup

synthNamedG :: (VividAction m, VarList params) => String -> params -> m NodeId
synthNamedG name params =
   makeSynth (UTF8.fromString name) params SCCmd.AddToHead defaultGroup

makeSynth :: (VividAction m, VarList params, IsNode node) => ByteString -> params -> SCCmd.AddAction -> node -> m NodeId
makeSynth theSynthName params addAction (getNodeId -> targetNodeId) = do
   nodeId <- newNodeId
   callOSC $ SCCmd.s_new theSynthName nodeId addAction targetNodeId paramList
   pure nodeId
 where
  -- HERE:
   paramList :: [(ByteString, Either Int32 Float)]
   paramList =
      [ (UTF8.fromString k, Right v) | (k, v) <- (fst $ makeTypedVarList params) ]


-- Can dedupe all these!:

-- TODO: instead of e.g. 'synthBefore', 'parGroupBefore' etc, can just do 'parGroup AddBefore'

-- | Create a synth at the head of the target group (see \"Order of Execution\")
newSynthAtHead
  , synthHead
  , synthOn
  , newSynthAtTail
  , synthTail
  :: (VividAction m, VarList params, Subset (InnerVars params) args, IsGroup group)
  => group -> SynthDef args -> params -> m (Synth args)
newSynthAtHead targetNode theSD params = do
   defineSD theSD
   Synth <$> makeSynth (synthName theSD) params SCCmd.AddToHead targetNode
-- | Alias for 'newSynthAtHead'
synthHead = newSynthAtHead
-- | Alias for 'newSynthAtHead'
synthOn = newSynthAtHead
-- | Create a synth at the tail of the target group (see \"Order of Execution\")
newSynthAtTail targetNode theSD params = do
   defineSD theSD
   Synth <$> makeSynth (synthName theSD) params SCCmd.AddToTail targetNode
-- | Alias for 'newSynthAtTail'
synthTail = newSynthAtTail

-- | Create a synth just before the target node (see \"Order of Execution\")
newSynthBefore
  , synthBefore
  , newSynthAfter
  , synthAfter
  :: (VividAction m, VarList params, Subset (InnerVars params) args, IsNode node)
  => node -> SynthDef args -> params -> m (Synth args)
newSynthBefore targetNode theSD params = do
   defineSD theSD
   Synth <$> makeSynth (synthName theSD) params SCCmd.AddBefore targetNode
-- | Alias for 'newSynthBefore'
synthBefore = newSynthBefore
-- | Create a synth just after the target node (see \"Order of Execution\")
newSynthAfter targetNode theSD params = do
   defineSD theSD
   Synth <$> makeSynth (synthName theSD) params SCCmd.AddAfter targetNode
-- | Alias for 'newSynthAfter'
synthAfter = newSynthAfter

-- | Stop the SuperCollider server
quitSCServer :: IO ()
quitSCServer = do
   callOSC $ SCCmd.quit
   closeSCServerConnection

-- | Synchronous
freeBuf :: VividAction m => BufferId -> m ()
freeBuf bufId = oscWSync $ \syncId ->
   callOSC $ SCCmd.b_free bufId (Just $ SCCmd.sync syncId)

newGroup :: VividAction m => m Group
newGroup = newGroupAtHead defaultGroup

newParGroup :: VividAction m => m ParGroup
newParGroup = newParGroupAtHead defaultGroup

newGroupBefore, newGroupAfter :: (IsNode node, VividAction m) => node -> m Group
newGroupBefore = makeGroup SCCmd.AddBefore
newGroupAfter = makeGroup SCCmd.AddAfter

newGroupAtHead, newGroupAtTail :: (IsGroup group, VividAction m) => group -> m Group
newGroupAtHead = makeGroup SCCmd.AddToHead
newGroupAtTail = makeGroup SCCmd.AddToTail

newParGroupBefore, newParGroupAfter :: (IsNode node, VividAction m) => node -> m ParGroup
newParGroupBefore = makeParGroup SCCmd.AddBefore
newParGroupAfter = makeParGroup SCCmd.AddAfter

newParGroupAtHead, newParGroupAtTail :: (IsGroup group, VividAction m) => group -> m ParGroup
newParGroupAtHead = makeParGroup SCCmd.AddToHead
newParGroupAtTail = makeParGroup SCCmd.AddToTail

makeGroup :: (IsNode target, VividAction m) => SCCmd.AddAction -> target -> m Group
makeGroup = makeSomeKindaGroup SCCmd.g_new Group

makeParGroup :: (IsNode target, VividAction m) => SCCmd.AddAction -> target -> m ParGroup
makeParGroup = makeSomeKindaGroup SCCmd.p_new ParGroup

type GroupCmd = NodeId -> SCCmd.AddAction -> NodeId -> OSC

makeSomeKindaGroup :: (IsNode target, VividAction m) => (GroupCmd) -> (NodeId -> group) -> SCCmd.AddAction -> target -> m group
makeSomeKindaGroup createCommand constructor addAction (getNodeId -> targetNode) = do
   nodeId <- newNodeId
   callOSC $ createCommand nodeId addAction targetNode
   sync
   pure $ constructor nodeId

