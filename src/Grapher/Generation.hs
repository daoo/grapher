module Grapher.Generation
  ( randomWorld
  , randomWorldIO
  ) where

import Control.Applicative
import Grapher.Particle
import Grapher.Types
import Grapher.Vector2F
import Grapher.World
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

arbitraryPoint :: Gen Point
arbitraryPoint = newVector2F <$> choose s <*> choose s
  where s = (-2000, 2000)

arbitraryParticle :: Gen Particle
arbitraryParticle = mkParticle <$> arbitraryPoint <*> pure 1 <*> pure 10

arbitraryLink :: Int -> Gen (Int, Int)
arbitraryLink n = (,) <$> choose (0, n-1) <*> choose (0, n-1)

arbitraryWorld :: Int -> Int -> Gen World
arbitraryWorld parts links =
  newWorld <$> vectorOf parts arbitraryParticle <*> vectorOf links (arbitraryLink parts)

arbitraryWorld1 :: Gen World
arbitraryWorld1 = sized $ \n -> do
  m <- choose (n, 2*n)
  arbitraryWorld n m

randomWorld :: Int -> Int -> World
randomWorld = unGen arbitraryWorld1 . mkQCGen

randomWorldIO :: IO World
randomWorldIO = generate arbitraryWorld1
