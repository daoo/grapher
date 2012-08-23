module ForceGraph.World where

import Control.Applicative
import Data.List
import ForceGraph.Backend.Backend
import ForceGraph.Ball
import ForceGraph.Circle
import ForceGraph.Physics
import ForceGraph.Time
import Math.Vector2
import Test.QuickCheck

data World = World
  { worldBoundary :: Circle
  , worldBalls :: [Ball]
  }
  deriving Show

instance Arbitrary World where
  arbitrary = liftA2 World arbitrary arbitrary

  shrink (World c objs) = map (World c) (tails objs)

showWorld :: World -> String
showWorld = unlines . map show . worldBalls

iteration :: Time -> World -> World
iteration t w = w { worldBalls = map f (worldBalls w) }
  where
    f = integrate t . limit w . updateBall (worldBalls w)

mulVec :: Num a => Vector2 a -> Vector2 a -> Vector2 a
mulVec (Vector2 ax ay) (Vector2 bx by) = Vector2 (ax * bx) (ay * by)

limit :: World -> Ball -> Ball
limit w b = b { vel = v' }
  where
    wc = circlePos $ worldBoundary w
    wr = circleRadius $ worldBoundary w
    br = radius b
    bp = pos b

    v' | bp `dist` wc > wr - br = mag (vel b) `mult` normalize (wc - bp)
       | otherwise              = vel b

updateBall :: [Ball] -> Ball -> Ball
updateBall objs obj = addVel obj (invMass obj `mult` repels)
  where
    repels = sum $ map (repel obj) objs

render :: Backend a => Settings -> Size -> World -> a ()
render set (w, h) world = do
  setColor $ getBgColor set
  fillRectangle zero (w, h)
  setColor $ getFgColor set
  strokeCircle (circleRadius $ worldBoundary world) (circlePos $ worldBoundary world)
  mapM_ f $ worldBalls world

  where
    f o = do
      setColor $ getFgColor set
      fillCircle (radius o) (pos o)
      setColor (0.937255, 0.160784, 0.160784)
      fillCircle (radius o * 0.8) (pos o)
