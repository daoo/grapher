module Extensions where

magic :: ((a, a) -> (a, a)) -> [a] -> [a]
magic _ []       = []
magic f (x : xs) = let (x', xs') = g x xs
                    in x' : magic f xs'
  where
    g y []       = (y, [])
    g y (z : zs) = let (y', z')   = f (y, z)
                       (y'', zs') = g y' zs
                    in (y'', z' : zs')
