module ForceGraph.Extensions where

clamp :: Ord a => a -> a -> a -> a
clamp a b x | x < a     = a
            | x > b     = b
            | otherwise = x
