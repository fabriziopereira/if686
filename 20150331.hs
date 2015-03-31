----------- questao 1 -------------

{- 
O polimorfismo de sobrecarga ocorre quando temos funções de mesmo nome, porém distintas para cada tipo. Em Java, isso é feito através de criação de 
métodos de mesmo nome, porém de assinaturas diferentes, seja pelo número ou tipo de argumentos. Já em Haskell, isso é feito através da utilização 
de classes de tipos, que permitem definir diferentes tipos para uma função ou operação. Dessa forma, se por um lado em Haskell fica mais simples 
definirmos uma função que utiliza classes de tipos para suportar a sobrecarga, ao invés de mais de um método de diferentes argumentos; por outro 
lado, acaba sendo mais custoso por ser necessário realizar o processo de unificação, que é a correspondência entre os argumentos e parâmetros passados.
Referências
CASTOR, Fernando. Polimorfismo. Disponível em: <https://docs.google.com/a/cin.ufpe.br/viewer?a=v&pid=sites&srcid=Y2luLnVmcGUuYnJ8aWY2ODZ8Z3g6NDYwNmI3ZGMxNDExMjFlOQ>. Acesso em: 28 mar. 2015.
MEDEIROS, Higor. Uso de Polimorfismo em Java. Disponível em: <http://www.devmedia.com.br/uso-de-polimorfismo-em-java/26140>. Acesso em: 28 mar. 2015.
OLIVEIRA, Guilherme Gomes Neves de; PETRI, Renzo Augusto Lapelligrini. Haskell: Seminário de Linguagens de Programação. Disponível em: <http://pt.slideshare.net/renzopetri/seminario-haskell>. Acesso em: 28 mar. 2015.
-}

----------- questao 2 -------------

quebraLook :: (Show t, Num t) => String -> t -> String
quebraLook (a:[]) n = (show n) ++ [a]
quebraLook (a:as) n
 | a == (head as) = quebraLook as (1+n)
 | otherwise = (show n) ++ [a] ++ quebraLook as 1

repeat' :: (Eq t, Num t) => String -> t -> String
repeat' s 1 = "1"
repeat' s 2 = quebraLook s 1
repeat' s n = repeat' (quebraLook s 1) (n-1)

lookAndSay :: (Eq t, Num t) => t -> String
lookAndSay n = repeat' "1" n

----------- questao 3 -------------

{-
Defina um tipo de dados que representa um grafo não necessariamente conexo, onde cada nó tem um rótulo:
type Grafo = [(vertice, [adjacencias])], ou seja, [(t, [t])]
-}

{-
Exemplo de grafo: [(1,[2,3]), (2,[1,3,4]), (3,[1,2,4]), (4,[2,3]), (5,[])]
-}

listaVertices :: (Eq t) => [(t,[t])] -> [(t,Bool)] -- constroi a lista de vertices com a flag booleana "visitado" = False, usando o grafo como entrada
listaVertices [] = []
listaVertices ((x,y):as) = [(x,False)] ++ (listaVertices as)

marcaVertices :: (Eq t) => [(t,Bool)] -> t -> Bool -> [(t,Bool)] -- altera "visitado" usando como entrada a lista de vertices, o vertice e True or False
marcaVertices [] vertice visitado = []
marcaVertices ((x,y):as) vertice visitado
    | x == vertice = [(x,visitado)] ++ (marcaVertices as vertice visitado)
    | otherwise = [(x,y)] ++ (marcaVertices as vertice visitado)

adjacentes :: (Eq t) => [(t,[t])] -> t -> [t] -- retorna a lista de vertices adjacents a um vertice t usando o grafo como entrada
adjacentes [] vertice = []
adjacentes ((x,y):as) vertice
    | x == vertice = y
    | otherwise = adjacentes as vertice

visitado :: (Eq t) => [(t,Bool)] -> t -> Bool -- retorna o estado de "visitado" de um vértice usando a lista de vertices como entrada
visitado [] vertice = False
visitado ((x,y):as) vertice
    | x == vertice = y
    | otherwise = visitado as vertice

proximoAdjacente :: (Eq t) => [t] -> [(t,Bool)] -> [t] -- retorna o proximo vertice adjacente não visitado, entradas: listas de adjacentes e de vertices
proximoAdjacente [] vertices = []
proximoAdjacente (a:as) vertices
    | visitado vertices a == True = proximoAdjacente as vertices
    | otherwise = [a]

criaCaminho :: (Eq t) => [t] -> [(t,t)] -- pega a pilha e remonta o caminho do vertice inicial ao desejado
criaCaminho [] = []
criaCaminho (a:as)
    | as == [] =  [] -- quando é o último elemento não se cria um caminho dele até o vazio
    | otherwise = (criaCaminho as)++[((head as), a)]

busca :: (Eq t) => [(t,[t])] -> [(t,Bool)] -> [t] -> t -> t -> [(t,t)] -- funcao de busca em profundidade
busca grafo vertices [] inicio fim = [] -- caso base de a pilha estar vazia (terem acabado os vertices adjacentes não visitados)
busca grafo vertices pilha inicio fim
    | (proximoAdjacente (adjacentes grafo inicio) vertices) == [] = busca grafo vertices (tail pilha) inicio fim -- não há adjacente válido, volta a pilha
    | head (proximoAdjacente (adjacentes grafo inicio) vertices) == fim = criaCaminho (fim:pilha) -- chegou no vertice pretendido, retorna o caminho
    | otherwise = busca grafo (marcaVertices vertices inicio True) ((proximoAdjacente (adjacentes grafo inicio) vertices)++pilha) (head (proximoAdjacente (adjacentes grafo inicio) vertices)) fim 
    -- marca o proximo vertice adjacente como visitado, o coloca na pilha e o considera como inicio

search :: (Eq t) => [(t,[t])] -> t -> t -> [(t,t)] -- funcao inicial que marca vertices como não lidos, define o incial como visitado e o coloca na fila
search grafo inicio fim = busca grafo (marcaVertices (listaVertices grafo) inicio True) [inicio] inicio fim

----------- questao 4 -------------

-- Ex: [[9,4,5,0,8],[10,3,2,1,7],[9,1,6,3,15],[0,3,8,10,1],[1,16,9,12,7]]

list :: [Int] -> Int -> [Int]
list [] _ = []								--Retorna uma lista com os n primeiros numeros
list (h:t) n | n > 0 = h:list t (n-1)
           | otherwise = list t n

fun :: [Int] -> Int -> Int -> [Int]       --Retorna a lista em um certo intervalo da matriz
fun (h:t) n tam = list (drop (n - (tam `div` 2) - 1) (h:t)) tam


listLinhas :: [[Int]] -> Int -> Int -> Int -> [[Int]]
listLinhas [] _ tam j  = []
listLinhas (h:t) n tam j | n > 0 = fun h j tam:listLinhas t (n-1) tam j
				           | otherwise =   listLinhas t (n) tam j


fun2 ::  [[Int]] -> Int -> Int -> Int -> [[Int]]  --Retorna as linhas de um certo intervalo da matriz
fun2 (h:t) n tam j = listLinhas (drop (n - (tam `div` 2) - 1) (h:t)) tam tam j



viz :: [[Int]] -> Int -> Int -> Int -> [[Int]]         --Retorna uma lista com o numero e seus vizinhos
viz [] _ _ _ = []
viz (h:t) i j tam = if (i > tam `div` 2 && j > tam `div` 2 && i < length (h:t) - tam `div` 2 + 1 && j < length (h:t) - tam `div` 2 + 1) 
		       then	fun2 (h:t) i tam j
		       else []

conc :: [[Int]]  -> [Int]           --Junta os numeros em uma unica lista
conc []  = []
conc (h:t) = h++conc t


qSort :: [Int] -> [Int]                      --quicksort
qSort [] = []
qSort (h:t) = qSort [b | b <- t, b < h] ++ [h] ++ qSort [b | b <- t, b >= h]

med :: [Int] -> Int
med [] = 0
med (h:t) = (head (drop (length (h:t) `div`2) (qSort (h:t))) )


--Geração da nova matriz
aux :: [[Int]] -> Int -> Int -> [Int]
aux []  _ _ = []
aux (h:t) i tam =[med (conc (viz (h:t) i k tam))| k <- [1..length(h:t)]]

fMediana :: [[Int]] -> Int -> [[Int]]
fMediana [] _ = []
fMediana (h:t) tam = [aux (h:t) i tam | i <- [1..length(h:t)]]       