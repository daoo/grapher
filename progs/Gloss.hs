{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Data.Monoid
import Grapher.Generation
import Grapher.Vector2F
import Grapher.World
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import qualified Graphics.Gloss as G

world :: World
world = unGen (arbitraryGrid 10 10) (mkQCGen 532453742) 100

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
render = mconcat . particlesWithLinks part link
  where
    link a b = G.line [vtup a, vtup b]
    part p   = uncurry G.translate (vtup p) $ G.circleSolid radius

radius :: Float
radius = 10
