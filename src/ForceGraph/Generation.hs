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
arbitraryPoint = (:+) <$> choose s <*> choose s
  where s = (-2000, 2000)

arbitraryParticle :: Gen Particle
arbitraryParticle = mkParticle <$> arbitraryPoint <*> pure 1

arbitraryBall :: Gen Ball
arbitraryBall = Ball
  <$> arbitraryParticle
  <*> pure 10
  <*> pure 10

arbitraryLinks :: Gen [(Int, Int)]
arbitraryLinks = sized $ \n -> mapM (\m -> (,) m <$> choose (m+1,n-1)) [0..n-1]

arbitraryWorld :: Gen World
arbitraryWorld = sized $ \n -> do
  balls <- vectorOf n arbitraryBall
  links <- arbitraryLinks
  return $ newWorld balls links

randomWorld :: Int -> IO World
randomWorld n = (\stdgen -> unGen arbitraryWorld stdgen n) `fmap` getStdGen
