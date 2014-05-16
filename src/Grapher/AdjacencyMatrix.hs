{-# LANGUAGE BangPatterns, TupleSections #-}
module Grapher.AdjacencyMatrix
  ( Matrix
  , newMatrix
  , isLinked
  , withLinked
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

setLink :: Int -> (Int, Int) -> STUArray s Int Bool -> ST s ()
setLink n (i, j) arr = do
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
          | otherwise = setLink n (i, j) arr >> go xs

{-# INLINE isLinked #-}
isLinked :: Matrix -> Int -> Int -> Bool
isLinked (Matrix n m) i j = m `unsafeAt` calcIx n i j

withLinked :: (Int -> Int -> b) -> Matrix -> [b]
withLinked f m@(Matrix n _) = go 0 0
  where
    go !i !j
      | i < n && j < n = if isLinked m i j then f i j : go i (j+1) else go i (j+1)
      | i < n          = go (i+1) 0
      | otherwise      = []
