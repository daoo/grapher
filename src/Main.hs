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
  100
  defaultWorld
  render
  (const (iteration . realToFrac))

render :: World -> G.Picture
render world =
  mconcat (map (\(i, j) -> line [at i, at j]) links) <>
  mconcat (map ball balls)

  where
    balls = worldBalls world
    links = worldLinks world

    at i            = position $ balls !! i
    t (Vector2 x y) = (realToFrac x, realToFrac y)

    line     = G.line . map t
    circle r = G.circleSolid (realToFrac r)

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

    ball b = uncurry G.translate (t $ position b) $ body
          <> mconcat (zipWith f colors (forces (worldBalls world) [] b))
          <> f G.magenta (accel (particle b))
      where
        body = G.color G.red $ circle (radius b)

        f c v = G.color c $ arrow zero v

        colors = [ G.aquamarine
                 , G.azure
                 , G.blue
                 , G.chartreuse
                 , G.cyan
                 , G.green
                 , G.orange
                 , G.rose
                 , G.violet
                 , G.yellow
                 ]
