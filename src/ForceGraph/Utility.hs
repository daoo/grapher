module ForceGraph.Utility where

times :: (a -> a) -> Int -> a -> a
times f 0 x = x
times f i x = times f (i-1) (f x)
