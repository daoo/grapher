module Extensions where

magic :: ((a, a) -> (a, a)) -> [a] -> [a]
magic _ []       = []
magic f (x : xs) = let (x', xs') = g x xs
                    in x' : magic f xs'
  where
    g x []       = (x, [])
    g x (y : ys) = let (x', y')   = f (x, y)
                       (x'', ys') = g x' ys
                    in (x'', y' : ys')
