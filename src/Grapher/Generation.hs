module Grapher.Generation
  ( arbitraryWorld
  , arbitraryGrid
  ) where

import Control.Applicative
import Grapher.Particle
import Grapher.Types
import Grapher.Vector2F
import Grapher.World
import Test.QuickCheck.Gen

arbitraryPoint :: Gen Point
arbitraryPoint = newVector2F <$> choose s <*> choose s
  where s = (-2000, 2000)

arbitraryParticle :: Gen Particle
arbitraryParticle = mkParticle <$> arbitraryPoint <*> pure 1 <*> pure 10

arbitraryLink :: Int -> Gen (Int, Int)
arbitraryLink n = (,) <$> choose (0, n-1) <*> choose (0, n-1)

-- |Generate a n-by-m grid.
arbitraryGrid :: Int -> Int -> Gen World
arbitraryGrid n m = do
  parts <- vectorOf (n*m) arbitraryParticle
  return $ newWorld parts (go 0 0)
  where
    go i j
      | i == n-1 && j == m-1 = []
      | i == n-1             = edger i j : go i (j+1)
      | j == m-1             = edged i j : go (i+1) 0
      | otherwise            = edger i j : edged i j : go i (j+1)

    edger i j = let a = ix i j in (a, a+1)
    edged i j = let a = ix i j in (a, a+m)

    ix i j = i*m+j

-- |Generate an completely arbitrary world with a specified number of particles
-- and links.
arbitraryWorld :: Int -> Int -> Gen World
arbitraryWorld parts links =
  newWorld <$> vectorOf parts arbitraryParticle <*> vectorOf links (arbitraryLink parts)
