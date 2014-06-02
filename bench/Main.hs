module Main (main) where

import Criterion.Main
import Grapher.Generation
import Grapher.World
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

world :: World
world = unGen (arbitraryWorld 10 500) (mkQCGen 532453742) 100

times :: Int -> (a -> a) -> a -> a
times 0 _ a = a
times i f a = times (pred i) f (f a)

main :: IO ()
main = defaultMain
  [ bench "100000 iterations" $ whnf (times 100000 (iteration 0.01)) world ]
