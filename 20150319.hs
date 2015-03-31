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

 {--}

fibo :: Int -> Int -- funcao que calcula o fibo de um num
fibo 0 = 0
fibo 1 = 1 
fibo n = fibo (n-1) + fibo (n-2)

fibPar :: Int -> Bool -- saber se fibo de n = par
fibPar n 
	|mod (fibo n) 2 == 0 = True
	|otherwise = False

fibP :: Int -> Int -> [Int] -- retorna os n primeiros num pares de fibo
fibP n i
	| n == 0 = []
	| fibPar (i) == True = [fibo(i)] ++ fibP (n-1) (i+1)
	| otherwise = fibP n (i + 1)         




