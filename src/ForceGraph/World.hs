module ForceGraph.World where

import Control.Applicative
import Data.Vector2
import ForceGraph.Backend.Backend
import ForceGraph.Ball
import ForceGraph.Extensions
import ForceGraph.Physics
import ForceGraph.Rectangle
import ForceGraph.Time
import Test.QuickCheck

type Link = (Int, Int)

data World = World
  { worldBoundary :: Rectangle
  , worldBalls :: [Ball]
  , worldLinks :: [Link]
  }
  deriving Show

instance Arbitrary World where
  arbitrary = liftA3 World arbitrary arbitrary arbitrary

showWorld :: World -> String
showWorld = unlines . map show . worldBalls

iteration :: Time -> World -> World
iteration t w = w { worldBalls = link (worldLinks w) $ map f (worldBalls w) }
  where
    f = integrate t
      . limitSpeed
      . limitPosition (worldBoundary w)
      . repelAcc t (worldBalls w)

link :: [Link] -> [Ball] -> [Ball]
link = flip (foldr f)
  where
    f (i, j) balls = if pos a `dist` pos b > 200 then balls' else balls
      where
        a = balls !! i
        b = balls !! j

        v = collision (vel a, mass a) (vel b, mass b)

        balls' = g i $ g j balls
        g = mapIndex (`setVel` v)

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
    repels = sum $ map (forceColumb (f obj) . f) objs
    f a = (pos a, charge a)

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
