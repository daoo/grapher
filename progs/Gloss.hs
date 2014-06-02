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
render w = mconcat (linkList link w) <> mconcat (map ball $ particleList w)
  where
    link a b = G.line [vtup a, vtup b]
    ball b   = uncurry G.translate (vtup b) $ G.circleSolid radius

radius :: Float
radius = 10
