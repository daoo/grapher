{-# LANGUAGE BangPatterns #-}
module Grapher.Generation
  ( grid
  , binaryTree
  ) where

import Control.Exception
import Control.Monad.State

-- |Generate an n-by-m grid.
grid :: Int -> Int -> (Int, [(Int, Int)])
grid n m = assert (n > 0 && m > 0) $ (n*m, go 0 0)
  where
    go !i !j
      | j >= m    = go (i+1) 0
      | i >= n    = []
      | otherwise = edges (i, j) $ go i (j+1)

    edges ix = edge a (edgel ix)
             . edge a (edger ix)
             . edge a (edged ix)
             . edge a (edgeu ix)
      where
        a = index ix

    edge a ix@(i, j) xs
      | i >= 0 && i < n && j >= 0 && j < m = (a, b) : xs
      | otherwise                          = xs

        where b = index ix

    edgel (i, j) = (i, j+1)
    edger (i, j) = (i, j-1)
    edged (i, j) = (i+1, j)
    edgeu (i, j) = (i-1, j)

    index (i, j) = i*m+j

-- |Generate an binary tree with pre-defined height.
binaryTree :: Int -> (Int, [(Int, Int)])
binaryTree height = assert (height > 0) $ (count, evalState (go 1 0) 1)
  where
    go :: Int -> Int -> State Int [(Int, Int)]
    go !h !i
      | h < height = do
        j <- free
        k <- free
        ls <- go (h+1) j
        rs <- go (h+1) k
        return $ (i, j) : (j, i) : (i, k) : (k, i) : (ls ++ rs)

      | otherwise = return []

    free = do
      n <- get
      put (n+1)
      return n

    count = sum $ map (2^) [0..(height-1)]
