{-# LANGUAGE BangPatterns, TupleSections #-}
-- |A data structure for an adjacency matrix. There is no direction for the
-- relation, that is the 'isAdjacent' function is commutative.
module Grapher.AdjacencyMatrix
  ( Matrix
  , newMatrix
  , isAdjacent
  , withAdjacent
  ) where

import Control.Monad.ST
import Data.Array.Base (unsafeAt, unsafeWrite)
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed (UArray)

data Matrix = Matrix !Int !(UArray Int Bool)
  deriving Show

calcIx :: Int -> Int -> Int -> Int
calcIx n i j = (i * n) + j

setAdjacent :: Int -> (Int, Int) -> STUArray s Int Bool -> ST s ()
setAdjacent n (i, j) arr = do
  unsafeWrite arr (calcIx n i j) True
  unsafeWrite arr (calcIx n j i) True

newMatrix :: Int -> [(Int, Int)] -> Matrix
newMatrix !n !links = Matrix n (runSTUArray (new >>= fill))
  where
    maxindex = n*n-1

    new :: ST s (STUArray s Int Bool)
    new = newArray (0, maxindex) False

    fill arr = go links
      where
        go [] = return arr

        go ((i, j):xs)
          | i >= n    = error "index too large"
          | j >= n    = error "index too large"
          | otherwise = setAdjacent n (i, j) arr >> go xs

{-# INLINE isAdjacent #-}
-- |Check if two indices are adjacent.
isAdjacent :: Matrix -> Int -> Int -> Bool
isAdjacent (Matrix n m) i j = m `unsafeAt` calcIx n i j

{-# INLINE withAdjacent #-}
-- |Map a function over all adjacent indices and collect the result in a list.
--
-- This function will result in duplicates, that is for all i, j both (i, j)
-- and (j, i) will be in the result.
withAdjacent :: (Int -> Int -> a) -> Matrix -> [a]
withAdjacent f m@(Matrix n _) = go 0 0
  where
    go !i !j
      | i < n && j < n = if isAdjacent m i j then f i j : go i (j+1) else go i (j+1)
      | i < n          = go (i+1) 0
      | otherwise      = []
