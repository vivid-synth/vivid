
# Vivid - *music and sound synthesis in Haskell*

Introductory video: https://www.youtube.com/watch?v=xo3zUvPsizo

Listen to examples at: http://vivid-synth.com

Haskell package: https://hackage.haskell.org/package/vivid

## Example usage:

```haskell
{-# LANGUAGE DataKinds #-}

import Vivid

theSound = sd (0 ::I "note") $ do
    wobble <- sinOsc (freq_ 5) ? KR ~* 10 ~+ 10
    s <- 0.1 ~* sinOsc (freq_ $ midiCPS (V::V "note") ~+ wobble)
    out 0 [s,s]

playSong = do
    fork $ do
        s0 <- synth theSound (36 ::I "note")
        wait 1
        free s0
    s1 <- synth theSound (60 ::I "note")
    forM_ [62,66,64] $ \note -> do
        wait (1/4)
        set s1 (note ::I "note")
    wait (1/4)
    free s1

main = do
    putStrLn "Simplest:"
    playSong

    putStrLn "With precise timing:"
    doScheduledIn 0.1 playSong
    wait 1

    putStrLn "Written to a file, non-realtime synthesis:"
    putStrLn "(Need to quit the running server for NRT)"
    quitSCServer
    writeNRT "/tmp/song.wav" playSong
```

## Installation

```bash
cabal install vivid
```
