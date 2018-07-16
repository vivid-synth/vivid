{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Vivid.Randomness (
     pick
   , picks
   , exprand
   , module System.Random.Shuffle
   ) where

import Control.Monad.Random (getRandomR, getRandomRs, MonadRandom)
import System.Random (Random)
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

exprand :: (Fractional n, Floating n, MonadRandom m, Random n) => n -> n -> m n
exprand lo hi = do
   r <- getRandomR (0, 1)
   pure $ lo * exp (log (hi / lo) * r)
