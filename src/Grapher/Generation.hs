{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
module Grapher.Generation
  ( Graph
  , grid
  , tree
  , circle
  ) where

import Control.Exception
import Control.Monad.State

type Graph = (Int, [(Int, Int)])

-- |Generate an n-by-m grid.
--
-- >>> grid 0 0
-- (0,[])
-- >>> grid 1 1
-- (1,[])
-- >>> grid 2 1
-- (2,[(0,1)])
-- >>> grid 1 2
-- (2,[(0,1)])
-- >>> grid 2 2
-- (4,[(0,1),(0,2),(1,3),(2,3)])
grid :: Int -> Int -> Graph
grid w h = (w*h, go 0 0)
  where
    go !x !y
      | y >= (h-1) = map (edger . (,y)) [0..(w-2)]
      | x >= (w-1) = edged (x,y) : go 0 (y+1)
      | otherwise = edger (x,y) : edged (x,y) : go (x+1) y

    edger ix@(x, y) = (index ix, index (x+1, y))
    edged ix@(x, y) = (index ix, index (x, y+1))

    index (x, y) = y*w+x

-- |Generate an n-ary tree with given height.
--
-- >>> tree 1 3
-- (3,[(0,1),(1,2)])
-- >>> tree 2 2
-- (3,[(0,1),(0,2)])
-- >>> tree 3 3
-- (13,[(0,1),(0,2),(0,3),(1,4),(1,5),(1,6),(2,7),(2,8),(2,9),(3,10),(3,11),(3,12)])
tree :: Int -> Int -> Graph
tree n height = assert (n > 0 && height > 1)
  (count, evalState (go 1 0) 1)
  where
    count = sum $ map (n^) [0..(height-1)]

    go :: Int -> Int -> State Int [(Int, Int)]
    go !h !i
      | h < height = do
        frees <- replicateM n free
        subtrees <- mapM (go (h+1)) frees
        return $ map (edge i) frees ++ concat subtrees

      | otherwise = return []

    free = do
      i <- get
      put (i+1)
      return i

    edge = (,)

-- |Generate a circle with n nodes.
--
-- >>> circle 0
-- (0,[])
-- >>> circle 1
-- (1,[])
-- >>> circle 2
-- (2,[(0,1)])
-- >>> circle 3
-- (3,[(0,1),(1,2),(2,0)])
-- >>> circle 4
-- (4,[(0,1),(1,2),(2,3),(3,0)])
circle :: Int -> Graph
circle 0 = (0, [])
circle 1 = (1, [])
circle 2 = (2, [(0,1)])
circle n = (n, map edge [0..(n-1)])
  where
    edge i = (i, (i+1) `mod` n)
