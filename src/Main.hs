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
  mconcat (map (\(i, j) -> line [index i, index j]) links)

  where
    balls = worldBalls world
    links = worldLinks world

    t (Vector2 x y) = (realToFrac x, realToFrac y)

    line     = G.line . map t
    circle r = G.circleSolid (realToFrac r)

    index i = position $ balls !! i

    arrow p d = line [p, q] <> line [a, b, c, a]
      where
        q = p + d
        l = mag d
        s = l * 0.03

        d'  = s .* normalize d
        d'' = orthogonal d'

        a = q + d'
        b = q + d''
        c = q - d''

    ball b = uncurry G.translate (t $ position b)
           $ f <> a <> body
      where
        body = G.color G.red  $ circle (radius b)

        a = G.color G.blue $ arrow zero (accel (particle b))
        f = G.color G.green $ arrow zero (forces world b)

simul :: Float -> World -> World
simul = iteration . realToFrac
