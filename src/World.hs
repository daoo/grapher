module World where

import Control.Arrow
import Data.List

import Test.QuickCheck.Arbitrary

import Backend.Backend

import Math.Vector2

import Object
import Physics

type Time = Double

type Pair = (Object, Object)
type Rope = (Int, Int)

data World = World
  { worldObjects :: [Object]
  , worldRopes :: [Rope] }
  deriving Show

instance Arbitrary World where
  arbitrary = do
    objs <- arbitrary
    conns <- arbitrary
    return $ World objs conns

showWorld :: World -> String
showWorld = intercalate "\n" . map show . worldObjects

iteration :: Time -> World -> World
iteration t (World objs cons) = World (go 0 objs) cons
  where
    go :: Int -> [Object] -> [Object]
    go _ []       = []
    go i (x : xs) = integrate t (object x objs (map (objs !!) $ asdf i cons)) : go (i + 1) xs

object :: Object -> [Object] -> [Object] -> Object
object obj objs cons = obj { vel = vel obj + (invMass obj `mult` totForce) }
  where
    repels = sum $ map (repel obj) objs
    drags  = sum $ map (drag obj) cons
    totForce = repels + drags

integrate :: Time -> Object -> Object
integrate t obj = obj { pos = pos obj + (t `mult` vel obj) }

asdf :: Int -> [Rope] -> [Int]
asdf i []                        = []
asdf i ((x, y) : zs) | x == i    = y : asdf i zs
                     | otherwise = asdf i zs

connect :: [Object] -> [Rope] -> [Pair]
connect xs = map ((xs !!) *** (xs !!))

render :: Backend a => Settings -> Size -> [Object] -> [Rope] -> a ()
render set (w, h) objs cons = do
  setColor $ getBgColor set
  fillRectangle zero (w, h)

  mapM_ g (connect objs cons)
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
