{-# LANGUAGE BangPatterns #-}
module Grapher.Generation
  ( Graph
  , grid
  , binaryTree
  , circle
  ) where

import Control.Exception
import Control.Monad.State

type Graph = (Int, [(Int, Int)])

-- |Generate an n-by-m grid.
grid :: Int -> Int -> Graph
grid n m = assert (n > 0 && m > 0) (n*m, go 0 0)
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
binaryTree :: Int -> Int -> Graph
binaryTree n height = assert (n > 0 && n > 0)
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

circle :: Int -> Graph
circle n = (n, concatMap f [0..(n-1)])
  where
    f i = let j = (i+1) `mod` n in [(i, j), (j, i)]
