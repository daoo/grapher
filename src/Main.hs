{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Monoid
import ForceGraph.Ball
import ForceGraph.Defaults
import ForceGraph.Particle
import ForceGraph.World
import Math.Vector2
import qualified Graphics.Gloss as G

main :: IO ()
main = G.simulate
  (G.InWindow "Force Graph" (800, 600) (0, 0))
  G.white
  60
  defaultWorld
  render
  (const simul)

render :: World -> G.Picture
render world =
  mconcat (map ball balls) <>
  mconcat (map (\(i, j) -> line (index i) (index j)) links)

  where
    balls = worldBalls world
    links = worldLinks world

    t (Vector2 x y) = (realToFrac x, realToFrac y)

    line x y = G.line [t x, t y]
    circle r = G.circleSolid (realToFrac r)

    index i = position $ balls !! i

    arrow p d = line p (p + d)

    ball b = uncurry G.translate (t $ position b)
           $ f <> a <> body
      where
        body = G.color G.red  $ circle (radius b)

        a = G.color G.blue $ arrow zero (accel (particle b))
        f = G.color G.green $ arrow zero (forces world b)

simul :: Float -> World -> World
simul = iteration . realToFrac
