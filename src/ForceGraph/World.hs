module ForceGraph.World where

import Data.List
import ForceGraph.Backend.Backend
import ForceGraph.Ball
import ForceGraph.Physics
import ForceGraph.Time
import Math.Vector2
import Test.QuickCheck

data World = World
  { worldLimitPos :: Vector2D
  , worldLimitRadius :: Double
  , worldBalls :: [Ball]
  }
  deriving Show

instance Arbitrary World where
  arbitrary = do
    x <- choose (100, 10000)
    y <- choose (100, 10000)
    r <- choose (100, 10000)
    b <- arbitrary
    return $ World (Vector2 x y) r b

  shrink (World p r objs) = map (World p r) (tails objs)

showWorld :: World -> String
showWorld = unlines . map show . worldBalls

iteration :: Time -> World -> World
iteration t w@(World p r objs) = World p r (map f objs)
  where
    f = integrate t . limit w . updateBall objs

mulVec :: Num a => Vector2 a -> Vector2 a -> Vector2 a
mulVec (Vector2 ax ay) (Vector2 bx by) = Vector2 (ax * bx) (ay * by)

limit :: World -> Ball -> Ball
limit w b = b { vel = v' }
  where
    wc = worldLimitPos w
    wr = worldLimitRadius w
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
  setColor $ (0, 0, 0)
  strokeCircle (worldLimitRadius world) (worldLimitPos world)
  mapM_ f $ worldBalls world

  where
    f o = do
      setColor $ getFgColor set
      fillCircle (radius o) (pos o)
      setColor (0.937255, 0.160784, 0.160784)
      fillCircle (radius o * 0.8) (pos o)

    g (o, c) = do
      setColor $ getFgColor set
      strokeLine (pos o, pos c)
