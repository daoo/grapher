module ForceGraph.World where

import Control.Applicative
import Data.List
import Data.Vector2
import ForceGraph.Backend.Backend
import ForceGraph.Ball
import ForceGraph.Physics
import ForceGraph.Rectangle
import ForceGraph.Time
import Test.QuickCheck

data World = World
  { worldBoundary :: Rectangle
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
    f = integrate t . limitSpeed . limitPosition (worldBoundary w) . repelAcc t (worldBalls w)

mulVec :: Num a => Vector2 a -> Vector2 a -> Vector2 a
mulVec (Vector2 ax ay) (Vector2 bx by) = Vector2 (ax * bx) (ay * by)

limitSpeed :: Ball -> Ball
limitSpeed b = if mag (vel b) > maxSpeed
  then setVel b $ maxSpeed `mult` normalize (vel b)
  else b
  where
    maxSpeed = 1000

limitPosition :: Rectangle -> Ball -> Ball
limitPosition rect b = b { pos = Vector2 px' py', vel = Vector2 vx' vy' }
  where
    r = radius b
    Vector2 px py = pos b
    Vector2 vx vy = vel b

    (px', vx') = f px vx $ rectWidth rect
    (py', vy') = f py vy $ rectHeight rect

    f p v s | p - r < 0 = (r, -v)
            | p + r > s = (s - r, -v)
            | otherwise = (p, v)

repelAcc :: Time -> [Ball] -> Ball -> Ball
repelAcc t objs obj = addVel obj ((t / mass obj) `mult` repels)
  where
    repels = sum $ map (repel obj) objs

render :: Backend a => Settings -> Size -> World -> a ()
render set (w, h) world = do
  setColor $ getBgColor set
  fillRectangle zero (w, h)
  mapM_ f $ worldBalls world

  where
    f o = do
      setColor $ getFgColor set
      fillCircle (radius o) (pos o)
      setColor (0.937255, 0.160784, 0.160784)
      fillCircle (radius o * 0.8) (pos o)
