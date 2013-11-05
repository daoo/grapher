{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Data.Monoid
import ForceGraph.Ball
import ForceGraph.Generation
import ForceGraph.Vector2F
import ForceGraph.World
import qualified Graphics.Gloss as G

main :: IO ()
main = do
  !world <- randomWorld 500
  G.simulate
    (G.InWindow "Force Graph" (800, 600) (0, 0))
    G.white
    100
    world
    render
    (const (iteration . realToFrac))

{-arrow :: Vector2F -> Vector2F -> G.Picture
arrow p d = G.line [vtup p, vtup q] <> G.line [vtup a, vtup b, vtup c, vtup a]
  where
    q = p + d
    l = mag d
    s = l * 0.03

    d'  = s .* normalize d
    d'' = orthogonal d'

    a = q + d'
    b = q + d''
    c = q - d''-}

render :: World -> G.Picture
render world = mconcat (linkBalls link world) <> mconcat (ballMap ball world)
  where
    link a b = G.line [vtup $ position a, vtup $ position b]
    ball b   = let Vector2F x y = position b in G.translate x y $ G.circleSolid (radius b)
