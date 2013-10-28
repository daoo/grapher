module Vector2
  ( Vector2()
  , vector2Eq
  , vectorTests
  ) where

import Control.Applicative
import Math.Vector2
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

floatEq :: (Fractional f, Ord f) => f -> f -> Bool
floatEq a b = abs (a - b) < 0.0001

vector2Eq :: (Fractional f, Ord f) => Vector2 f -> Vector2 f -> Bool
vector2Eq a b = mag2 (a - b) < 0.0001

instance Arbitrary a => Arbitrary (Vector2 a) where
  arbitrary = Vector2 <$> arbitrary <*> arbitrary

  shrink (Vector2 x y) = [Vector2 x' y' | x' <- shrink x, y' <- shrink y]
                      ++ [Vector2 x y' | y' <- shrink y]
                      ++ [Vector2 x' y | x' <- shrink x]

propAddCom :: Vector2 Int -> Vector2 Int -> Bool
propAddCom a b = a + b == b + a

propAddAssoc :: Vector2 Int -> Vector2 Int -> Vector2 Int -> Bool
propAddAssoc a b c = a + (b + c) == (a + b) + c

propAddIdentity :: Vector2 Int -> Bool
propAddIdentity a = a + zero == a

propAddInverse :: Vector2 Int -> Bool
propAddInverse a = negate a + a == zero

propSubAddInv :: Vector2 Int -> Vector2 Int -> Bool
propSubAddInv a b = a - b == a + (negate b)

propMulDistVec :: Int -> Vector2 Int -> Vector2 Int -> Bool
propMulDistVec s a b = s .* (a + b) == s .* a + s .* b

propMulDistField :: Int -> Int -> Vector2 Int -> Bool
propMulDistField s t a = (s + t) .* a == s .* a + t .* a

propMulCompatVecField :: Int -> Int -> Vector2 Int -> Bool
propMulCompatVecField s t a = s .* (t .* a) == (s * t) .* a

propMulIdentity :: Vector2 Int -> Bool
propMulIdentity a = 1 .* a == a

propDivInvMul :: NonZero Rational -> Vector2 Rational -> Bool
propDivInvMul (NonZero s) a = a ./ s == (1 / s) .* a

propDotCom :: Vector2 Int -> Vector2 Int -> Bool
propDotCom a b = a `dot` b == b `dot` a

propDotDistAdd :: Vector2 Int -> Vector2 Int -> Vector2 Int -> Bool
propDotDistAdd a b c = a `dot` (b + c) == a `dot` b + a `dot` c

propNormalizeMag1 :: Vector2 Double -> Property
propNormalizeMag1 a = a /= zero ==> floatEq 1 (mag (normalize a))

propOrthogonal :: Vector2 Int -> Bool
propOrthogonal a = (a `dot` orthogonal a) == 0

vectorTests :: Test
vectorTests = testGroup "2d vector"
  [ testProperty "addition commutativity" propAddCom
  , testProperty "addition associativity" propAddAssoc
  , testProperty "addition identity" propAddIdentity
  , testProperty "addition inverse" propAddInverse
  , testProperty "subtraction inverse addition" propSubAddInv
  , testProperty "scalar multiplication distributivity over vector addition" propMulDistVec
  , testProperty "scalar multiplication distributivity over field addition" propMulDistField
  , testProperty "scalar multiplication compatibilty with field multiplication" propMulCompatVecField
  , testProperty "scalar multiplication identity" propMulIdentity
  , testProperty "scalar division inverse multiplication" propDivInvMul
  , testProperty "dot product commutativity" propDotCom
  , testProperty "dot product distributivity over addition" propDotDistAdd
  , testProperty "normalization magnitude" propNormalizeMag1
  , testProperty "orthagonality" propOrthogonal
  ]
