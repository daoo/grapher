{-# LANGUAGE BangPatterns #-}
module Grapher.Util where

imap :: (Int -> a -> b) -> [a] -> [b]
imap f = go 0
  where
    go  _ []     = []
    go !i (x:xs) = f i x : go (i+1) xs
