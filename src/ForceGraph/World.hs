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
    f = integrate t . limitVelocity . limitPosition w . updateBall (worldBalls w)

mulVec :: Num a => Vector2 a -> Vector2 a -> Vector2 a
mulVec (Vector2 ax ay) (Vector2 bx by) = Vector2 (ax * bx) (ay * by)

limitVelocity :: Ball -> Ball
limitVelocity b = if mag (vel b) > maxSpeed
  then setVel b $ maxSpeed `mult` normalize (vel b)
  else b
  where
    maxSpeed = 1000

limitPosition :: World -> Ball -> Ball
limitPosition w b = if d > 0
  then b { pos = p', vel = v' }
  else b
  where
    wc = circlePos $ worldBoundary w
    wr = circleRadius $ worldBoundary w
    br = radius b
    bp = pos b

    tc  = wc - bp
    tcn = normalize tc

    d  = bp `dist` wc - wr + br
    v' = mag (vel b) `mult` tcn
    p' = bp + (d + br) `mult` tcn

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
