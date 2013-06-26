module ForceGraph.World where

import Control.Applicative
import ForceGraph.Backend.Backend
import ForceGraph.Ball
import ForceGraph.Extensions
import ForceGraph.Rectangle
import Math.Vector2
import Test.QuickCheck

type Link = (Int, Int)

data World = World
  { worldBoundary :: Rectangle
  , worldBalls :: [Ball]
  , worldLinks :: [Link]
  } deriving Show

mapBalls :: ([Ball] -> [Ball]) -> World -> World
mapBalls f w = w { worldBalls = f (worldBalls w) }

setBoundary :: Rectangle -> World -> World
setBoundary r w = w { worldBoundary = r }

instance Arbitrary World where
  arbitrary = liftA3 World arbitrary arbitrary arbitrary

showWorld :: World -> String
showWorld = unlines . map show . worldBalls

iteration :: Double -> World -> World
iteration t w = id
  $ mapBalls (map $ limitPosition (worldBoundary w))
  $ mapBalls (map $ integrate t)
  $ w

limitPosition :: Rectangle -> Ball -> Ball
limitPosition rect b = b { pos = Vector2 px' py' }
  where
    Vector2 px py = pos b

    px' = clamp 0 (width rect) px
    py' = clamp 0 (height rect) py

render :: Backend a => Settings -> Size -> World -> a ()
render set (w, h) world = do
  setColor $ getBgColor set
  fillRectangle zero (w, h)
  setColor $ getFgColor set
  mapM_ g $ map (\(i, j) -> (balls !! i, balls !! j)) links
  mapM_ f balls

  where
    balls = worldBalls world
    links = worldLinks world

    f o = do
      setColor $ getFgColor set
      fillCircle (radius o) (pos o)
      setColor (0.937255, 0.160784, 0.160784)
      fillCircle (radius o * 0.8) (pos o)

    g (a, b) = strokeLine (pos a, pos b)
