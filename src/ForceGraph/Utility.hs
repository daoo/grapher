module ForceGraph.Utility where

times :: (a -> a) -> Int -> a -> a
times f 0 x = x
times f i x = times f (i-1) (f x)

clamp :: Ord a => a -> a -> a -> a
clamp a b x | x < a     = a
            | x > b     = b
            | otherwise = x

mapIndex :: (Int -> a -> b) -> [a] -> [b]
mapIndex f = go 0
  where
    go i []     = []
    go i (x:xs) = f i x : go (i+1) xs
