module ForceGraph.Generation
  ( randomWorld ) where

import Control.Applicative
import ForceGraph.Ball
import ForceGraph.Particle
import ForceGraph.Types
import ForceGraph.Vector2F
import ForceGraph.World
import System.Random
import Test.QuickCheck.Gen

arbitraryPoint :: Gen Point
arbitraryPoint = Vector2F <$> choose (-1000, 1000) <*> choose (-1000, 1000)

arbitraryParticle :: Gen Particle
arbitraryParticle = Particle <$> arbitraryPoint <*> arbitraryPoint <*> pure zero

arbitraryBall :: Gen Ball
arbitraryBall = Ball
  <$> arbitraryParticle
  <*> pure 10
  <*> pure 1
  <*> pure 10

arbitraryLinks :: Int -> Gen [(Int, Int)]
arbitraryLinks n = mapM (\m -> (\a -> (m, a)) <$> choose (m+1,n-1)) [0..n-1]

arbitraryWorld :: Gen World
arbitraryWorld = do
  balls <- listOf arbitraryBall
  links <- arbitraryLinks (length balls)
  return $ newWorld balls links

randomWorld :: IO World
randomWorld = (\stdgen -> unGen arbitraryWorld stdgen 100) `fmap` getStdGen
