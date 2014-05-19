module Grapher.Generation
  ( randomWorld
  , randomWorldIO
  ) where

import Control.Applicative
import Grapher.Ball
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

arbitraryBall :: Gen Ball
arbitraryBall = Ball
  <$> arbitraryParticle
  <*> pure 10

arbitraryLink :: Int -> Gen (Int, Int)
arbitraryLink n = (,) <$> choose (0, n-1) <*> choose (0, n-1)

arbitraryWorld :: Int -> Int -> Gen World
arbitraryWorld balls links =
  newWorld <$> vectorOf balls arbitraryBall <*> vectorOf links (arbitraryLink balls)

arbitraryWorld1 :: Gen World
arbitraryWorld1 = sized $ \n -> do
  m <- choose (n, 2*n)
  arbitraryWorld n m

randomWorld :: Int -> Int -> World
randomWorld gen n = unGen arbitraryWorld1 (mkQCGen gen) n

randomWorldIO :: IO World
randomWorldIO = generate arbitraryWorld1
