venda  :: Int -> Int
venda n
 | n == 0 = 3
 | otherwise = 6
valor :: Int -> Int -> Int
valor s n
 | n == 0 && (venda 0) == s = 1
 | n == 0 = 0 
 | (venda n) == s = (valor s (n-1)) + 1
 | otherwise = (valor s (n -1))

