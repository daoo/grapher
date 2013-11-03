{-# LANGUAGE BangPatterns #-}
module ForceGraph.LinkMatrix
  ( Matrix()
  , newMatrix
  , isLinked
  , withLinked
  ) where

import Control.Exception
import Control.Monad.ST
import Data.Array.Base (unsafeAt, unsafeWrite)
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed (UArray)

newtype Matrix = Matrix (Int, UArray Int Bool)
  deriving Show

calcIx :: Int -> Int -> Int -> Int
calcIx n i j = (i * n) + j

{-# INLINE isLinked #-}
isLinked :: Matrix -> Int -> Int -> Bool
isLinked (Matrix (n, m)) i j = m `unsafeAt` calcIx n i j

newMatrix :: Int -> [(Int, Int)] -> Matrix
newMatrix n links = Matrix (n, runSTUArray (new >>= go links))
  where
    new :: ST s (STUArray s Int Bool)
    new = newArray (0, n*n-1) False

    go []          arr = return arr
    go ((i, j):xs) arr = do
      assert (i < n && j < n) (return ())
      unsafeWrite arr (calcIx n i j) True
      unsafeWrite arr (calcIx n j i) True
      go xs arr

withLinked :: (Int -> a) -> (a -> a -> b) -> Matrix -> [b]
withLinked lu f m@(Matrix (n, _)) = go 0 0
  where
    go !i !j | j < n     = if isLinked m i j then f (lu i) (lu j) : go i (j+1) else go i (j+1)
             | i < n     = go (i+1) i
             | otherwise = []
