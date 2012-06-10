module Extensions where

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
mapCombinations f xs = concatMap g xs
  where g a = map (f a) xs

mapIndex :: (a -> a) -> Int -> [a] -> [a]
mapIndex _ _ []       = []
mapIndex f 0 (x : xs) = f x : xs
mapIndex f i (x : xs) = x : mapIndex f (i - 1) xs

times :: (a -> a) -> Int -> a -> a
times f i x | i == 0    = x
            | i < 0     = error "Negative times"
            | otherwise = times f (i - 1) (f x)
