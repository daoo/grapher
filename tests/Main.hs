module Main (main) where

import Grapher.AdjacencyMatrix
import Test.Tasty
import Test.Tasty.QuickCheck as QC

toList :: Matrix -> [(Int, Int)]
toList = withLinked (,)

allLinks :: Int -> [(Int, Int)]
allLinks n = [ (i, j) | i <- [0..n-1], j <- [0..n-1] ]

prop_toListAllEqual :: Positive Int -> Bool
prop_toListAllEqual (Positive n) = toList (newMatrix n links) == links
  where
    links = allLinks n

tests :: TestTree
tests = testGroup "Properties"
  [ testGroup "Link Matrix"
    [ testProperty "To list all equal" prop_toListAllEqual
    ]
  ]

main :: IO ()
main = defaultMain tests
