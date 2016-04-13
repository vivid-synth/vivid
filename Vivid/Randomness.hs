{-# LANGUAGE NoMonomorphismRestriction #-}

module Vivid.Randomness (
     pick
   , picks
   , module System.Random.Shuffle
   ) where

import Control.Monad.Random (getRandomR, getRandomRs, MonadRandom)
import System.Random.Shuffle

-- | Picks a random element from the provided list
pick :: MonadRandom m => [a] -> m a
pick l = (l !!) <$> getRandomR (0, (length::[a]->Int) l - 1)

-- | Returns an infinite list of randomly-chosen elements from the provided list
-- 
--   e.g.
-- 
--   >> > take 5 <$> picks [1,2,3,4]
--   >> [2,3,1,1,3]
picks :: (MonadRandom m) => [a] -> m [a]
picks l =
   (map (l !!)) <$> getRandomRs (0, (length::[a]->Int) l - 1)
