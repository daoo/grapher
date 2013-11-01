{-# LANGUAGE BangPatterns #-}
module ForceGraph.EdgeMatrix
  ( Matrix
  , newMatrix
  , (!)
  ) where

import Data.Array.BitArray hiding ((!))

type Matrix = BitArray (Int, Int)

newMatrix :: [[Bool]] -> Matrix
newMatrix list = array ((0,0), (r,c)) (go 0 0 list)
  where
    r = length list
    c = length (head list)

    go  _  _ []          = []
    go !i  _ ([]:xs)     = go (i+1) 0 xs
    go !i !j ((y:ys):xs) = ((i, j), y) : go i (j+1) (ys:xs)

(!) :: Matrix -> (Int, Int) -> Bool
(!) = (!!!)
