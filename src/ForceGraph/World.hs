module ForceGraph.World where

import Data.List
import ForceGraph.Backend.Backend
import ForceGraph.Object
import ForceGraph.Physics
import Math.Vector2
import Test.QuickCheck.Arbitrary

type Time = Double

type Pair = (Object, Object)

data World = World
  { worldObjects :: [Object] }
  deriving Show

instance Arbitrary World where
  arbitrary = do
    objs <- arbitrary
    return $ World objs

  shrink (World objs) = map World (tails objs)

showWorld :: World -> String
showWorld = intercalate "\n" . map show . worldObjects

iteration :: Time -> World -> World
iteration t (World objs) = World (go 0 objs)
  where
    go :: Int -> [Object] -> [Object]
    go _ []       = []
    go i (x : xs) = integrate t (object x objs) : go (i + 1) xs

object :: Object -> [Object] -> Object
object obj objs = obj { vel = vel obj + (invMass obj `mult` repels) }
  where
    repels = sum $ map (repel obj) objs

integrate :: Time -> Object -> Object
integrate t obj = obj { pos = pos obj + (t `mult` vel obj) }

render :: Backend a => Settings -> Size -> [Object] -> a ()
render set (w, h) objs = do
  setColor $ getBgColor set
  fillRectangle zero (w, h)
  mapM_ f objs

  where
    f o = do
      setColor $ getFgColor set
      fillCircle 10 (pos o)
      setColor (0.937255, 0.160784, 0.160784)
      fillCircle 8 (pos o)

    g (o, c) = do
      setColor $ getFgColor set
      strokeLine (pos o, pos c)
