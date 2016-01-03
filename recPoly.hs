--all use l where xs would be traditional

--57 characters
skips :: [a] -> [[a]]
skips l = [[l !! j | j <- [i-1, i-1+i..length l-1]] | i <- [1..length l]]

--uses True where otherwise would be traditional and f instead of localMaxima
--changed type signature to be more generic e.g. localMaxima "abcba" -- "c"
--66 characters
localMaxima :: Ord a => [a] -> [a]
localMaxima = f where
  f (x:y:z:l)
    | x < y
    , y > z 
      = y : f (y:z:l)
    | True = f (y:z:l)
  f _ = [] 

--136 characters
histogram :: [Integer] -> String
histogram l = unlines [[if i >= n then '*' else ' ' | i <- c] | n <- [m+1,m..1]] ++ "==========\n0123456789\n"
  where c = [length $ filter (== n) l | n <- [0..9]]
        m = maximum c
