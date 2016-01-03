digits :: Int -> [Int]
digits x | x < 0 = []
	 | otherwise = digits (x `div` 10) ++ [x `mod` 10]

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther (x:y:xs) = x : 2 * y : doubleEveryOther xs
doubleEveryOther (x) = x

validate = (== 0) . (`mod` 10) . sum . doubleEveryOther . digits
