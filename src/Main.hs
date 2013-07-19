{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Math.Vector2
import Data.Monoid
import ForceGraph.Ball
import ForceGraph.Defaults
import ForceGraph.World
import Graphics.Gloss

main :: IO ()
main = simulate
  (InWindow "Force Graph" (800, 600) (0, 0))
  white
  60
  defaultWorld
  render
  simul

render :: World -> Picture
render world =
  (mconcat $ map ball balls) <>
  (mconcat $ map (\(i, j) -> line [p i, p j]) links) <>
  (color white blank)

  where
    balls = worldBalls world
    links = worldLinks world

    p i = t $ position $ balls !! i

    t (Vector2 x y) = (realToFrac x, realToFrac y)

    ball b = color red $ uncurry translate (t $ position b) $ circleSolid (realToFrac $ radius b)

simul :: t -> Float -> World -> World
simul _ t w = iteration (realToFrac t) w
