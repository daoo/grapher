module ForceGraph.World where

import Data.List
import ForceGraph.Backend.Backend
import ForceGraph.Ball
import ForceGraph.Physics
import ForceGraph.Time
import Math.Vector2
import Test.QuickCheck

data World = World
  { size :: Size
  , worldBalls :: [Ball]
  }
  deriving Show

instance Arbitrary World where
  arbitrary = do
    w <- choose (100, 10000)
    h <- choose (100, 10000)
    b <- arbitrary
    return $ World (w, h) b

  shrink (World s objs) = map (World s) (tails objs)

showWorld :: World -> String
showWorld = unlines . map show . worldBalls

iteration :: Time -> World -> World
iteration t (World s objs) = World s (map f objs)
  where
    f = limit s . integrate t . updateBall objs

mulVec :: Num a => Vector2 a -> Vector2 a -> Vector2 a
mulVec (Vector2 ax ay) (Vector2 bx by) = Vector2 (ax * bx) (ay * by)

limit :: Size -> Ball -> Ball
limit (w, h) b = b { vel = v' `mulVec` vel b }
  where
    r = radius b
    Vector2 px py = pos b

    v' | px <= r || px + r >= w = Vector2 0 1
       | py <= r || py + r >= h = Vector2 1 0
       | otherwise              = Vector2 1 1

updateBall :: [Ball] -> Ball -> Ball
updateBall objs obj = addVel obj (invMass obj `mult` repels)
  where
    repels = sum $ map (repel obj) objs

render :: Backend a => Settings -> Size -> [Ball] -> a ()
render set (w, h) objs = do
  setColor $ getBgColor set
  fillRectangle zero (w, h)
  mapM_ f objs

  where
    f o = do
      setColor $ getFgColor set
      fillCircle (radius o) (pos o)
      setColor (0.937255, 0.160784, 0.160784)
      fillCircle (radius o * 0.8) (pos o)

    g (o, c) = do
      setColor $ getFgColor set
      strokeLine (pos o, pos c)
