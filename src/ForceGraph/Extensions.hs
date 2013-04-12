module ForceGraph.Extensions where

-- |Magic no duplication pairing of all elements in a list
magic :: ((a, a) -> (a, a)) -> [a] -> [a]
magic _ []       = []
magic f (x : xs) = let (x', xs') = g x xs
                    in x' : magic f xs'
  where
    g y []       = (y, [])
    g y (z : zs) = let (y', z')   = f (y, z)
                       (y'', zs') = g y' zs
                    in (y'', z' : zs')

foldlAll :: (a -> a -> a) -> [a] -> [a]
foldlAll f xs = map (\x -> foldl f x xs) xs

mapCombinations :: (a -> a -> b) -> [a] -> [b]
mapCombinations f xs = concatMap ((`map` xs) . f) xs

mapIndex :: (a -> a) -> Int -> [a] -> [a]
mapIndex f i xs | i < 0 = undefined
                | otherwise = go i xs
  where
    go _ []     = []
    go 0 (y:ys) = f y : ys
    go j (y:ys) = y : go (j - 1) ys

times :: (a -> a) -> Int -> a -> a
times f i x | i < 0     = undefined
            | otherwise = go i x
  where
    go 0 y = y
    go j y = go (j - 1) (f y)
