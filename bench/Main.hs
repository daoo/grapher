{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Criterion.Main
import Grapher.Generation
import Grapher.World

world :: World
world = uncurry newWorld (grid 15 15)

times :: Int -> (a -> a) -> a -> a
times  0 _ a = a
times !i f a = times (pred i) f (f a)

main :: IO ()
main = defaultMain
  [ bench "1000 iterations" $ whnf (times 1000 (iteration 0.01)) world ]
