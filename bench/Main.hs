module Main (main) where

import Criterion.Main
import Grapher.Generation
import Grapher.World

world :: World
world = randomWorld 52430753 10000

times :: Int -> (a -> a) -> a -> a
times 0 _ a = a
times i f a = times (pred i) f (f a)

main :: IO ()
main = defaultMain
  [ bench "1000000 iterations" $ whnf (times 1000000 (iteration 0.01)) world ]
