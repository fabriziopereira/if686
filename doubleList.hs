double :: [Int] -> [Int]
double list
 | list == [] = []
 | otherwise = ((head list)*2) : (double (tail list))