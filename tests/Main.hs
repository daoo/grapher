module Main (main) where

import ForceGraph.Utility
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

propTimes :: NonNegative Int -> Bool
propTimes (NonNegative i) = times (+1) 0 i == i

tests :: [Test]
tests =
  [ testGroup "misc"
    [ testProperty "Times successor" propTimes ]
  ]

main :: IO ()
main = defaultMain tests
