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

-- |Generate an n-ary tree with given height.
binaryTree :: Int -> Int -> (Int, [(Int, Int)])
binaryTree n height = assert (n > 0 && n > 0) $
  (count, evalState (go 1 0) 1)
  where
    go :: Int -> Int -> State Int [(Int, Int)]
    go !h !i
      | h < height = do
        frees <- replicateM n free
        subtrees <- mapM (go (h+1)) frees
        return $ concatMap (\j -> [(i, j), (j,i)]) frees ++ concat subtrees

      | otherwise = return []

    free = do
      i <- get
      put (i+1)
      return i

    count = sum $ map (n^) [0..(height-1)]
