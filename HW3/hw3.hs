-- Writen by: Xiaomeng Cao

-- Problem 1
rmChar :: Char -> String -> String
rmChar c str = [x | x <- str, c /= x]



-- Problem 2
rmCharsRec :: String -> String -> String
rmCharsRec _ []      = []
rmCharsRec [] s2     = s2
rmCharsRec (s:s1) s2 = rmChar s (rmCharsRec s1 s2)

rmCharsFold :: String -> String -> String
rmCharsFold xs ys = foldr (rmChar) ys xs



-- Problem 3
andRec :: [Bool] -> Bool
andRec []     = True
andRec (x:bs) = x && andRec bs

andFold :: [Bool] -> Bool
andFold bs = foldr (&&) True bs



-- Problem 4
same :: [Int] -> Bool
same xs = and (zipWith (==) xs (tail xs))
