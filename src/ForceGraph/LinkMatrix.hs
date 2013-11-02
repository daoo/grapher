{-# LANGUAGE BangPatterns #-}
module ForceGraph.LinkMatrix
  ( Matrix()
  , newMatrix
  , isLinked
  , withLinked
  ) where

import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed

newtype Matrix = Matrix (Int, UArray Int Bool)
  deriving Show

{-# INLINE isLinked #-}
isLinked :: Matrix -> Int -> Int -> Bool
isLinked (Matrix (n, m)) i j = m `unsafeAt` ((i * n) + j)

buildSquare :: (Int -> Int -> a) -> Int -> Int -> [[a]]
buildSquare f a b = [[f i j | i <- [a..b]] | j <- [a..b]]

newMatrix :: Int -> [(Int, Int)] -> Matrix
newMatrix n links = Matrix (n, a)
  where
    a = listArray (0, n*n-1) (concat $ buildSquare elem' 0 (n-1))

    elem' i j = elem (i, j) links || elem (j, i) links

withLinked :: (Int -> a) -> (a -> a -> b) -> Matrix -> [b]
withLinked lu f m@(Matrix (n, _)) = go 0 0
  where
    go !i !j | j < n     = if isLinked m i j then f (lu i) (lu j) : go i (j+1) else go i (j+1)
             | i < n     = go (i+1) i
             | otherwise = []
