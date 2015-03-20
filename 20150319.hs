double :: [Int] -> [Int]
double list
 | list == [] = []
 | otherwise = ((head list)*2) : (double (tail list))
 
 {--}
 
member :: [Int] -> Int-> Bool
member membros membro
 | membros == [] = False
 | head membros == membro = True
 | otherwise = member (tail membros) membro
 
 {--}
 
num :: String -> Char -> Bool
num s c
 | s == [] = False
 | (head s) == c = True
 | otherwise = num (tail s) c

digits :: String -> String
digits s
 | s == [] = []
 | num ['0'..'9'] (head s) = (head s) : digits (tail s)
 | otherwise = digits (tail s)
 
{--}

sumPairs :: [Int] -> [Int] -> [Int]
sumPairs x y
 | x == [] = y
 | y == [] = x
 | otherwise = (head x) + (head y) : sumPairs (tail x) (tail y) 
 
 {--}
 
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (h : t) =
 quickSort [y | y <- t, y < h] ++ [h] ++ quickSort[y | y <- t, y >= h]
