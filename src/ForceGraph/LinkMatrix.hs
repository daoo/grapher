{-# LANGUAGE BangPatterns #-}
module ForceGraph.LinkMatrix
  ( Matrix()
  , newMatrix
  , isLinked
  ) where

import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed

newtype Matrix = Matrix (Int, UArray (Int, Int) Bool)

{-# INLINE isLinked #-}
isLinked :: Matrix -> Int -> Int -> Bool
isLinked (Matrix m) i j = snd m `unsafeAt` ((i * fst m) + j)

newMatrix :: Int -> [(Int, Int)] -> Matrix
newMatrix n links = Matrix (n, listArray ((0, 0), (n-1,n-1)) (go 0 0))
  where
    go !i !j | j < n     = elem' i j links : go i (j+1)
             | i < n     = go (i+1) 0
             | otherwise = []

    elem' _ _ []                                                       = False
    elem' i j ((i', j'):xs) | i == i' && j == j' || i == j' && j == i' = True
                            | otherwise                                = elem' i j xs
