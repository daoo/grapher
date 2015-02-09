{-# LANGUAGE BangPatterns, TupleSections #-}
-- |A data structure for an adjacency matrix.
module Grapher.AdjacencyMatrix
  ( Matrix
  , newMatrix
  , isAdjacent
  , adjacentTo
  , withAdjacent
  ) where

import Control.Monad.ST
import Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable as M

-- TODO: Tight packing

data Matrix = Matrix !Int !(Vector Bool)
  deriving Show

calcIx :: Int -> Int -> Int -> Int
calcIx n i j = (i * n) + j

setAdjacent :: Int -> (Int, Int) -> MVector s Bool -> ST s ()
setAdjacent n (i, j) arr = unsafeWrite arr (calcIx n i j) True

-- |Create a new matrix from a list of adjacencies.
-- prop> withAdjacent (,) (newMatrix n (allLinks n)) == allLinks n
newMatrix :: Int -> [(Int, Int)] -> Matrix
newMatrix !n !links = Matrix n (create (mkvector >>= fill))
  where
    count = n*n-1

    mkvector :: ST s (MVector s Bool)
    mkvector = M.replicate count False

    fill arr = go links
      where
        go [] = return arr

        go ((!i, !j):xs)
          | i >= n    = error "index too large"
          | j >= n    = error "index too large"
          | otherwise = setAdjacent n (i, j) arr >> go xs

{-# INLINE isAdjacent #-}
-- |Check if two indices are adjacent.
isAdjacent :: Matrix -> Int -> Int -> Bool
isAdjacent (Matrix n m) i j = m `unsafeIndex` calcIx n i j

{-# INLINE adjacentTo #-}
-- |Get the indices that are adjacent to a given index.
adjacentTo :: (Int -> a) -> Matrix -> Int -> [a]
adjacentTo f (Matrix n m) i = go 0 (i*n)
  where
    go !k !x
      | k < n     = if m `unsafeIndex` x then f k : go (k+1) (x+1) else go (k+1) (x+1)
      | otherwise = []

{-# INLINE withAdjacent #-}
-- |Map a function over all adjacent indices and collect the result in a list.
withAdjacent :: (Int -> Int -> a) -> Matrix -> [a]
withAdjacent f (Matrix n m) = go 0 0 0
  where
    go !i !j !k
      | i < n && j < n = if m `unsafeIndex` k then f i j : go i (j+1) (k+1) else go i (j+1) (k+1)
      | i < n          = go (i+1) 0 k
      | otherwise      = []
