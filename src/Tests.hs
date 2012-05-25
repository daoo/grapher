module Tests where

import Vector2
import Test.QuickCheck

floatEq :: Float -> Float -> Bool
floatEq a b = (abs $ a - b) < 0.000001

propDotEqualAbsSquared :: Vector2 Integer -> Bool
propDotEqualAbsSquared u = (u `dot` u) == magSquared u

propAddCommutative :: Vector2 Integer -> Vector2 Integer -> Bool
propAddCommutative u v = u + v == v + u

propAddAssociative :: Vector2 Integer -> Vector2 Integer -> Vector2 Integer -> Bool
propAddAssociative u v w = u + (v + w) == (u + v) + w

propNormalizedLength :: Vector2F -> Bool
propNormalizedLength u = let m = mag (normalize u)
                          in m == 0 || (m `floatEq` 1)

propDistance :: Vector2F -> Vector2F -> Bool
propDistance u v = let d1 = u `dist` v
                       d2 = mag (v - u)
                    in d1 `floatEq` d2