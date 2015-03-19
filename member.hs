member :: [Int] -> Int-> Bool
member membros membro
 | membros == [] = False
 | head membros == membro = True
 | otherwise = member (tail membros) membro
