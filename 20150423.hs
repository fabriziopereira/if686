--------------- Trabalho 09 ---------------

---------------  Questão 01 ---------------

----- 01  intro programacao funcional -----

{-- Crie um função que recebe uma lista de inteiros e 
retorna a lista ordenada em função da soma de seus 
digitos(crescente):
Prelude> ordenar [5,12,70,8,25,3,150]
		[12,3,5,150,70,25,8] --}

qsortDigits :: [Int] -> [Int]
qsortDigits [] = []
qsortDigits (a:as) = qsortDigits menor ++ [a] ++ qsortDigits maior
    where menor  = [ x | x <- as, sumDigits x < sumDigits a ]
          maior = [ x | x <- as, sumDigits x >= sumDigits a ]

sumDigits :: Int -> Int
sumDigits 0 = 0
sumDigits x = sumDigits (x `div` 10) + (x `mod` 10)

ordenar :: [Int] -> [Int]
ordenar as = qsortDigits as

------- 02 Tuplas Casamento Padroes -------
-- tinha feito mas esquecido de dar push --

{-- Defina a função menorMaior que recebe
três inteiros e retorna uma tupla com o 
menor e o maior deles --}

menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior x y z
    | (x >= y) && (y >= z) = (z, x) -- x >= y >= z
    | (x >= z) && (z >= y) = (y, x) -- x >= z >= y
    | (y >= x) && (x >= z) = (z, y) -- y >= x >= z
    | (y >= z) && (z >= x) = (x, y) -- y >= z >= x
    | (z >= x) && (x >= y) = (y, z) -- z >= x >= y
    | otherwise = (x, z) -- caso restante de z >= y >= x 

{-- • Defina a função ordenaTripla que 
recebe uma tripla de inteiros e ordena a 
mesma --}

ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (x, y, z)
    | (x >= y) && (y >= z) = (z, y, x) -- x >= y >= z
    | (x >= z) && (z >= y) = (y, z, x) -- x >= z >= y
    | (y >= x) && (x >= z) = (z, x, y) -- y >= x >= z
    | (y >= z) && (z >= x) = (x, z, y) -- y >= z >= x
    | (z >= x) && (x >= y) = (y, x, z) -- z >= x >= y
    | otherwise = (x, y, z) -- caso restante de z >= y >= x 

{-- Uma linha pode ser representada da seguinte
forma:
type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)
   	Defina funções que 
	– retornem 
	• a primeira coordenada de um ponto
	• a segunda coordenada de um ponto
	– indique se uma reta é vertical ou não
	x1 == x2: --}

type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

xPonto :: (Ponto) -> Float
xPonto (x, y) = x

yPonto :: (Ponto) -> Float
yPonto (x, y) = y

vertical :: (Reta) -> Bool
vertical ((x1, y1), (x2, y2))
    | x1 == x2 = True
    | otherwise = False

{-- Se uma reta é dada por defina uma função (y -y1)/(x-x1)= (y2-y1)/(x2-x1)
pontoY :: Float -> Reta -> Float que, dadas
uma coordenada x e uma reta, retorne a coordenada y, 
tal que o ponto (x, y) faça parte da reta.
– o que acontece se a reta for vertical? --}

pontoY :: Float -> Reta -> Float -- se a reta for vertical, retornara infinito
pontoY x ((x1, y1), (x2,y2)) = (((y2-y1) / (x2-x1)) * (x-x1)) + y1

{-- Defina uma função que ordena uma lista de inteiros utilizando o algoritmo quick sort --}

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (h : t) =
 quickSort [y | y <- t, y < h] ++ [h] ++ quickSort[y | y <- t, y >= h]

{-- não tinha feito --}
{-- Redefina as seguintes funções utilizando compreensão de listas
membro :: [Int] -> Int -> Bool
livros :: BancoDados -> Pessoa -> [Livro]
emprestimos :: BancoDados -> Livro ->[Pessoa]
emprestado :: BancoDados -> Livro -> Bool
qtdEmprestimos :: BancoDados -> Pessoa -> Int
devolver :: 
 BancoDados -> Pessoa -> Livro -> BancoDados
--}

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]

baseExemplo :: BancoDados -- livros emprestados
baseExemplo = [("Sergio", "O Senhor dos Aneis"), ("Andre", "Duna"), ("Fernando", "Johnathan Strange & Mr. Norrel"), ("Fernando", "A Game of Thrones")]

livros :: BancoDados -> Pessoa -> [Livro]
livros bd p = [livro | (pessoa,livro) <- bd, p == pessoa]

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos bd l = [pessoa | (pessoa,livro) <- bd, l == livro]

emprestado :: BancoDados -> Livro -> Bool
emprestado bd l
    | emprestimos bd l == [] = False
    | otherwise = True

length' :: [Livro] -> Int
length' [] = 0
length' (a:as) = (length' as) + 1

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos bd p = length' (livros bd p)

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar bd p l = (p, l) : bd

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver bd p l = [ (pessoa,livro) | (pessoa,livro) <- bd, not ((l == livro) && (p == pessoa))]

------ 03-polimorfismo ------

{-- Crie uma função agrupar que recebe uma lista de listas de
valores de um tipo t que podem ser comparados para saber se 
são iguais e devolve uma lista de pares (t, Int) onde o primeiro 
elemento é um valor do tipo t que existe em pelo menos uma 
das sub-listas da entrada e o segundo é o número de ocorrências 
desse valor nas sub-listas: 
 Prelude> agrupar [“Fernando”, “Castor”] 
 [( 'F', 1), ('e', 1), ('r', 2), ('n', 2), ('a', 2), ('d', 1), ('o', 2), ('C', 1), 
('s', 1), ('t', 1)] 
 Prelude> agrupar [[4,2,4,3,4,4,4,5,4,6], [1,2,3,4,5],[2]] 
 [(4, 7), (2, 3), (3, 2), (5, 2), (6, 1), (1, 1)] 
 Prelude> agrupar [[(+), (-)], [agrupar]] 
 ??????????? --}

contaNumaLista :: Eq t => Int -> t -> [t] -> Int
contaNumaLista n _ [] = n 
contaNumaLista n x (a:as)
    | x == a = contaNumaLista (n+1) x as
    | otherwise = contaNumaLista n x as

conta :: Eq t => Int -> t -> [[t]] -> Int
conta n x [] = n
conta n x (a:as) = conta (n + (contaNumaLista 0 x a)) x as

retirar :: Eq t => t -> [[t]] -> [[t]]
retirar x [] = []
retirar x ([]:as) = retirar x as
retirar x ((b:bs):as)
    | x == b = retirar x (bs:as)
    | otherwise = [b] : retirar x (bs:as)

agrupar :: Eq t => [[t]] -> [(t, Int)]
agrupar [] = []
agrupar ([]:as) = agrupar as
agrupar ((b:bs):as) = (b, (conta 0 b ((b:bs):as))) : agrupar (retirar b ((b:bs):as))

-------- 04-tipos_algebricos ----------

--- Tinha feito e não tinha dado push ---
{-- Como definir a função abaixo?
	area :: Shape -> Float--}

data Shape = Circle Float
			| Rectangle Float Float
area :: Shape -> Float
area (Circle r) = (pi * (r*r))
area (Rectangle b a) = b*a

{-- Defina um tipo algébrico cujos valores sejam os dias 
da semana. Dias úteis devem ter associados a eles um 
número correspondente à quantidade de horas-aula 
daquele dia e uma lista de Strings correspondentes 
aos nomes das disciplinas daquele dia. Defina em 
seguida 
	(i) uma função que recebe um dia da semana e diz se é ou não fim-de-
	semana 
	(ii) uma função que informa se há ou não aula de PLC naquele dia. --}

data Dia = Segunda Int [String] | Terca Int [String] | Quarta Int [String] | Quinta Int [String] | Sexta Int [String] | Sabado | Domingo

ehFDS :: Dia -> Bool
ehFDS Domingo = True
ehFDS Sabado = True
ehFDS (Segunda h dis) = False
ehFDS (Terca h dis) = False
ehFDS (Quarta h dis) = False
ehFDS (Quinta h dis) = False
ehFDS (Sexta h dis) = False


temPLC :: Dia -> Bool
temPLC Sabado = False
temPLC Domingo = False
temPLC (Segunda h dis) = contido dis
temPLC (Terca h dis) = contido dis
temPLC (Quarta h dis) = contido dis
temPLC (Quinta h dis) = contido dis
temPLC (Sexta h dis) = contido dis

contido :: [String] -> Bool
contido [] = False
contido (a:as)
	| a == "PLC" = True
	| otherwise = contido as

--- não tinha feito ---
{-- Modifique o tipo Tree para que valores desse tipo sejam
comparáveis (para saber se são iguais) e que seja possível 
transformá-los em Strings --}

ata Tree t = NilT | Node t (Tree t) (Tree t) 

showTree :: Show t => Tree t -> String
showTree NilT = []
showTree (Node no (esquerda) (direita)) = (show no) ++ showTree(esquerda) ++ showTree(direita)

eqTree :: Eq t => Tree t -> Tree t -> Bool
eqTree NilT NilT = True
eqTree NilT _ = False
eqTree _ NilT = False
eqTree (Node no1 (esquerda1) (direita1)) (Node no2 (esquerda2) (direita2))
    | (no1 == no2) = (eqTree esquerda1 esquerda2) && (eqTree direita1 direita2)

{-- Defina as seguintes funções --}

data Expr = Lit Int | Add Expr Expr | Sub Expr Expr

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add e1 e2) = (showExpr e1) ++ "+" ++ (showExpr e2)
showExpr (Sub e1 e2) = (showExpr e1) ++ "-" ++ (showExpr e2)

data List t = Nil | Cabeca t (List t) deriving (Show)

toList :: List t -> [t]
toList Nil = []
toList (Cabeca x (y)) = [x] ++ (toList y)

fromList :: [t] -> List t
fromList [] = Nil
fromList (a:as) = (Cabeca a (fromList as))

data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Eq, Show)

depth :: Tree t -> Int
depth NilT = 0
depth t = percorre t (-1) -- raiz tem depth 0

percorre :: Tree t -> Int -> Int
percorre NilT n = n
percorre (Node v e d) n = max (percorre e (n+1)) (percorre d (n+1))

bfs :: Eq t => Tree t -> t -> Bool
bfs NilT n = False
bfs (Node v e d) n
 | v == n = True
 | otherwise = (||) (bfs e n) (bfs d n)
 
mapTree :: (t -> u) -> Tree t -> Tree u
mapTree f NilT = NilT
mapTree f (Node v e d) = (Node (f v) (mapTree f e) (mapTree f d))

------ 05-funcoes_alta_ordem ------

{-- Defina uma função que, dada uma lista de
Strings, a transforma em uma lista de números 
onde cada número dessa lista corresponde à soma 
dos “valores” dos caracteres do String que 
aparece na mesma posição da lista de entrada. Os 
valores correspondem à posição da letra no alfabeto 
('a' = 1, 'b' = 2, 'c' = 3, etc.). A função ord (do 
pacote Data.Char) pode ser útil para resolver essa 
questão.
Use tanto map quanto foldr para resolver a questão. --}

listaSoma :: [String] -> [Int]
listaSoma palavras = map somaCaracteres palavras

somaCaracteres :: String -> Int
somaCaracteres palavra = foldr (+) 0 (posicaoAlfabeto palavra)

{-- Defina em Haskell uma função para inserir um 
elemento em uma árvore de busca (não precisa ser 
balanceada). 
Em seguida, usando foldr, defina uma função que 
cria uma árvore a partir de uma lista de números 
inteiros e de uma função que insere elementos em uma 
árvore:
criarArvore :: 
 Ord t => [t]->(Tree t->t->Tree t)->Tree t --}

 a = Node 8 (Node 3 (Node 1 NilT NilT) (Node 6 (Node 4 NilT NilT) (Node 7 NilT NilT))) (Node 10 NilT (Node 14 (Node 13 NilT NilT) NilT))

inserir:: Ord t => t -> Tree t -> Tree t
inserir elemento NilT = (Node elemento NilT NilT)
inserir elemento (Node valor esquerda direita) 
    | elemento > valor = (Node valor esquerda (inserir elemento direita)) 
    | elemento < valor = (Node valor (inserir elemento esquerda) direita)

criarArvore :: Ord t => [t] -> (t -> Tree t -> Tree t) -> Tree t
criarArvore lista funcao = foldr funcao NilT (reverse lista) -- o reverse foi utilizado para a raiz ser o primeiro elemento da lista, nao o ultimo

-- criarArvore [8,3,1,6,4,7,10,14,13] inserir

{-- Dada uma função f do tipo t -> u -> v, defina uma expressão da forma 
(\... -> ...) 
 para uma função do tipo u -> t -> v
que se comporta como f mas recebe seus 
argumentos na ordem inversa --}

inverte :: (t -> u -> v) -> (u -> t -> v)
inverte f = (\ x y -> f y x)

{-- Defina, usando lambdas, funções para
	• Dada uma lista de pares, devolver uma lista contendo apenas os 
	primeiros elementos de cada par
	• Dados uma lista de listas de números e um número n, devolver 
	uma lista contendo todas cujo comprimento seja maior que n
	• Dada uma lista de listas, criar uma lista que contém todos os 
	elementos das sub-listas da lista de entrada, mas removendo 
	duplicação:
	Prelude> f [[1..10], [5..20], [6, 8.. 15]]
	[1,2,3,4,5,7,9,11,13,15,16,17,18,19,20,6,8,10,12,14] --}

primeirosPares :: [(t, u)] -> [t]
primeirosPares lista = map (\ (x,y) -> x) lista

comprimentoMaior :: [[Int]] -> Int -> [[Int]]
comprimentoMaior listas n = filter (\x -> (length x > n)) listas

uniao :: (Eq t) => (Ord t) => [[t]] -> [t]
uniao listas = removeRepeticao (sort ((foldr (\ x y -> x ++ y) [] listas)))

sort :: (Eq t) => (Ord t) => [t] -> [t]
sort [] = []
sort (a:as) = sort [b | b <- as, b < a] ++ [a] ++ sort [b | b <- as, b >= a]

removeRepeticao :: (Eq t) => [t] -> [t]
removeRepeticao [] = []
removeRepeticao (a:as)
    | (as /= []) && (a == head as) = removeRepeticao as
    | otherwise = a : removeRepeticao as

{-- Implemente uma função mapfold que, sem usar map
nem foldr, comporte-se como a função
map.foldr, fazer uma que se comporte como map.filter antes --}
mapfilter :: (t -> Bool) -> [[t]] -> [[t]]
mapfilter ffilter [] = []
mapfilter ffilter (a:as) = [y | y <- a, ffilter y] : (mapfilter ffilter as)
-- (mapfilter) (>(2)) [[1,2,3,4],[2,3,4,5],[1,2,3,4]] -- resultado: [[3,4],[3,4,5],[3,4]]

mapfold :: (a1 -> a -> a) -> [a] -> [[a1] -> a]
mapfold funcao lista = [(f x) | x <- lista]
	where
		f acumulador [] = acumulador
		f acumulador (a:as) = f (funcao a acumulador) as

func :: Bool -> Int -> Int -- funcao para teste
func True n = n + 10
func _ n = n - 5

-- [ f [True, False] | f <- ((mapfold) func [1,2,10])] -- resultado: [6,7,15]
---     (a:as / [])                 (funcao) (lista / valor da lista = acumulador)

---- 06-aplicacao_parcial_funcoes ----

{-- Defina, usando aplicação parcial de funções,
funções para:
	• Somar uma constante x a todos os elementos de uma 
	lista de números
	• Dada uma lista de números, obter o maior da lista
	• Dada uma árvore binária, devolver uma função 
	que, dada uma árvore binária, verifica se as duas 
	árvores são isomórficas
	• Dada uma lista, devolve uma função que, dada uma 
	lista, devolve uma lista de pares onde cada par 
	contém um elemento de cada lista --}

somaNumero :: [Int] -> Int -> [Int]
somaNumero lista n = map (+ n) lista

maiorNumero :: [Int] -> Int
maiorNumero lista = foldr (max) 0 lista

iso1 = Node 1 (Node 2 (Node 3 NilT NilT) NilT) (Node 4 NilT (Node 5 NilT NilT))
iso2 = Node 3 (Node 2 (Node 5 NilT NilT) NilT) (Node 7 NilT (Node 4 NilT NilT))
iso3 = Node 3 (Node 2 NilT NilT) (Node 7 NilT NilT)

isomorficas :: (Eq t) => Tree t -> (Tree t -> Bool)
isomorficas NilT NilT = True
isomorficas (Node valor1 esquerda1 direita1) (Node valor2 esquerda2 direita2)
    | ((esquerda1 == NilT) && (esquerda2 /= NilT) || (esquerda1 /= NilT) && (esquerda2 == NilT))= False
    | ((direita1 == NilT) && (direita2 /= NilT) || (direita1 /= NilT) && (direita2 == NilT))= False
    | otherwise = (isomorficas esquerda1 esquerda2) && (isomorficas direita1 direita2)

listaPares :: [t] -> ([t] -> [(t,t)])
listaPares [] _ = []
listaPares _ [] = []
listaPares (a:as) (b:bs) = (a,b) : listaPares as bs

---------------  Trabalho 09 ----------------
----------------  Questão 02 ----------------

data Grafos = Grafo [(Int, [(Int,Double)])] deriving (Show, Eq) -- grafo representado por vertice e lista de (adjacencente, peso)
-- foi definido na questao que o rotulo seria obrigatoriamente um int, entao fizemos uma modificacao para tirar tambem o Nil que nao seria mais necessario
-- alem disso, foi preciso alterar o valor dos pesos para um Double, para utilizar o algoritmo de djkstra que inicia distancias com um infinito (Double)
-- para um grafo não direcionado assumimos que na entrada se tiver uma adjacencia de numero1 para numero2, havera tambem de numero2 para numero1 
-- para facilitar o algoritmo consideramos uma tabela de (vertice, distancia minima para chegar ate ele, precedente)
-- referencia: http://www.inf.ufsc.br/grafos/temas/custo-minimo/dijkstra.html

g = Grafo ([(0,[(1,10),(2,2)]), (1,[(0,10),(2,4),(3,5)]), (2,[(0,2),(1,4),(4,3)]), (3,[(1,5),(4,2)]), (4,[(2,3),(3,2)])]) -- exemplo

h = Grafo ([(0,[(1,10),(2,2)]), (1,[(0,10),(2,4)]), (2,[(0,2),(1,4),(4,3)]), (3,[]), (4,[(2,3)])]) -- exemplo

infinito = 1/0

formaTabela :: Grafos -> Int -> [(Int, Double, Int)] -- forma a tabela usada no algoritmo colocando a distancia do vertice inicial = 0
formaTabela (Grafo []) _ = []
formaTabela (Grafo ((x,y):as)) inicial
    | x == inicial = (x, 0, x) : (formaTabela (Grafo as) inicial) -- se for o vertice inicial, a distancia sera 0
    | otherwise = (x, infinito, (-1)) : (formaTabela (Grafo as) inicial) -- ao contrario, a principio distancia = infinito e o precedente = -1

defineAbertos :: Grafos -> [Int] -- pega os vertices do grafo para a primeira definicao de quais estao abertps
defineAbertos (Grafo []) = []
defineAbertos (Grafo ((vertice, y):as)) = vertice : (defineAbertos (Grafo (as)))

marcaFechado :: [Int] -> (Int, Double) -> [Int] -- marca um vertice como fechado
marcaFechado abertos (vertice, distanica) = [x | x <- abertos, x /= vertice]

menorDistancia :: [(Int, Double, Int)] -> [Int] -> Double -- retorna a menor distancia existente na tabela entre os vertices abertos
menorDistancia tabela abertos = foldr (min) infinito ([y | (x,y,z) <- tabela, (elem x abertos)])

verticeMenorDistancia :: [(Int, Double, Int)] -> [Int] -> (Int, Double)
verticeMenorDistancia tabela abertos = head ([(x,y) | (x,y,z) <- tabela, y == (menorDistancia tabela abertos)])

adjacentes :: Grafos -> (Int,Double) -> [(Int, Double)] -- retorna a lista de vertices adjacentes e seus pesos usando o grafo como entrada
adjacentes (Grafo []) _ = []
adjacentes (Grafo ((x,y):as)) (vertice,peso)
    | x == vertice = y
    | otherwise = adjacentes (Grafo as) (vertice,peso)

auxTabela :: [(Int, Double, Int)] -> (Int, Double) -> [Int] -> (Int, Double) -> [(Int, Double, Int)] -- altera distancia e precedente caso melhor caminho
auxTabela [] _ _ _ = []
auxTabela ((vertice1, distanciaAtual, precendete):as) (anterior, distancia) abertos (vertice2,peso)
    | (vertice1 == vertice2) && (elem vertice1 abertos) && ((peso + distancia) < distanciaAtual) =  ((vertice1, (peso + distancia), anterior):as)
    | otherwise = (vertice1, distanciaAtual, precendete) : (auxTabela as (anterior, distancia) abertos (vertice2,peso))

alteraTabela :: [(Int, Double, Int)] -> (Int, Double) -> [Int] -> [(Int, Double)] -> [(Int, Double, Int)] -- atualiza os adjacentes usando auxTabela
alteraTabela tabela _ _ [] = tabela
alteraTabela tabela verticeAnterior abertos ((vertice,peso):as)
    | as == [] = auxTabela tabela verticeAnterior abertos (vertice,peso)
    | otherwise = alteraTabela (auxTabela tabela verticeAnterior abertos (vertice,peso)) verticeAnterior abertos as

formaCaminho :: [(Int, Double, Int)] -> [(Int, Double, Int)] -> Int -> Int -> [Int] -- forma o caminho a partir dos precedentes
formaCaminho [] _ _ _ = []
formaCaminho ((vertice, distancia, precedente):as) tabelaOficial inicio posicao
    | posicao == inicio = [posicao]
    | vertice == posicao = (formaCaminho tabelaOficial tabelaOficial inicio precedente) ++ [vertice]
    | otherwise = formaCaminho as tabelaOficial inicio posicao

verificaCaminho :: [Int] -> Int -> Int -> [Int] -- verifica se o caminho formado é válido
verificaCaminho (a:as) inicio fim
    | a == inicio = (a:as)
    | otherwise = [] -- se o primeiro elemento não for o inicio, o caminho não existe

dijkstra :: Grafos -> [(Int, Double, Int)] -> [Int] -> (Int, Double) -> [(Int, Double, Int)] -- algoritmo de dijkstra
dijkstra grafo tabela abertos anterior
	| ((length abertos) == 1) = alteraTabela tabela anterior abertos (adjacentes grafo (verticeMenorDistancia tabela abertos)) 
	| otherwise = dijkstra grafo (alteraTabela tabela anterior abertos (adjacentes grafo (verticeMenorDistancia tabela abertos))) (marcaFechado abertos (verticeMenorDistancia tabela abertos)) (verticeMenorDistancia (alteraTabela tabela anterior abertos (adjacentes grafo (verticeMenorDistancia tabela abertos))) (marcaFechado abertos (verticeMenorDistancia tabela abertos)))
 
geraFuncaoMenorCaminho :: Grafos -> (Int -> Int -> [Int]) -- funcao principal
geraFuncaoMenorCaminho grafo inicio fim = verificaCaminho (formaCaminho (dijkstra grafo (formaTabela grafo inicio) (defineAbertos grafo) (inicio, 0.0)) (dijkstra grafo (formaTabela grafo inicio) (defineAbertos grafo) (inicio, 0.0)) inicio fim) inicio fim
