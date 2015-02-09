{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Data.Monoid
import Grapher.AdjacencyMatrix
import Grapher.Generation
import Grapher.Particle
import Grapher.Vector2F
import Grapher.World
import qualified Data.Vector.Unboxed as V
import qualified Graphics.Gloss as G

world :: World
--world = uncurry newWorld (binaryTree 1 50)
--world = uncurry newWorld (binaryTree 4 5)
world = uncurry newWorld (grid 10 10)
--world = uncurry newWorld (Grapher.Generation.circle 50)

main :: IO ()
main = do
  G.simulate
    (G.InWindow "Force Graph" (800, 600) (0, 0))
    G.white
    100
    world
    render
    (const iteration)

render :: World -> G.Picture
render w = mconcat $
  map part (V.toList (worldNodes w)) ++
  withAdjacent (\i j -> link (particle w i) (particle w j) ) (worldEdges w)
  where
    link pa pb = G.line [vtup $ pos pa, vtup $ pos pb]
    part p     = uncurry G.translate (vtup $ pos p) $ G.circleSolid radius

radius :: Float
radius = 10
