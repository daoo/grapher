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

foldAll :: (a -> a -> a) -> [a] -> [a]
foldAll f xs = map (\x -> foldl f x xs) xs

mapIndex :: (a -> a) -> Int -> [a] -> [a]
mapIndex _ _ []       = []
mapIndex f 0 (x : xs) = f x : xs
mapIndex f i (x : xs) = x : mapIndex f (i - 1) xs
