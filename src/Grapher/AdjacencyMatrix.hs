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
import Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable as M

data Matrix = Matrix !Int !(Vector Bool)
  deriving Show

calcIx :: Int -> Int -> Int -> Int
calcIx n i j = (i * n) + j

setAdjacent :: Int -> (Int, Int) -> MVector s Bool -> ST s ()
setAdjacent n (i, j) arr = do
  unsafeWrite arr (calcIx n i j) True
  unsafeWrite arr (calcIx n j i) True

newMatrix :: Int -> [(Int, Int)] -> Matrix
newMatrix !n !links = Matrix n (create (mkvector >>= fill))
  where
    count = n*n-1

    mkvector :: ST s (MVector s Bool)
    mkvector = M.replicate count False

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
isAdjacent (Matrix n m) i j = m `unsafeIndex` calcIx n i j

{-# INLINE withAdjacent #-}
-- |Map a function over all adjacent indices and collect the result in a list.
--
-- This function will result in duplicates, that is for all i, j both (i, j)
-- and (j, i) will be in the result.
withAdjacent :: (Int -> Int -> a) -> Matrix -> [a]
withAdjacent f (Matrix n m) = go 0 0 0
  where
    go !i !j !k
      | i < n && j < n = if m `unsafeIndex` k then f i j : go i (j+1) (k+1) else go i (j+1) (k+1)
      | i < n          = go (i+1) 0 k
      | otherwise      = []
