—— TRABALHO 10 ——


1. Determine os tipos das expressões a seguir. É necessário mostrar o desenvolvimento que levou você dos tipos das sub-expressões ao tipo da expressão final (por exemplo, como você saiu dos tipos de map, foldr, (.) e (+) para chegar ao tipo de foldr (+).(.).map).


foldr (+).(.).map
Colocando parêntesis para ficar mais fácil:
(foldr (+)).((.).map)

foldr(+):
foldr :: (a -> b -> b) -> b -> [a] -> b
(+) :: Num c => c -> c -> c
(a -> b -> b) = c -> c -> c
a = c
b = c
foldr (+) :: Num c => c -> [c] -> c

(.).map:
(.) :: (e -> f) -> (d -> e) -> d -> f
(.) :: (h -> i) -> (g -> h) -> g -> i
map :: (j -> k) -> [j] -> [k]
(h -> i) = (e -> f) -> (d -> e) -> d -> f
h = (e -> f)
i = (d -> e) -> d -> f
(g -> h) = (j -> k) -> [j] -> [k]
g = (j -> k)
h = [j] -> [k]
igualando h:
e = [j]
f = [k]
(.).map: (j -> k) -> (d -> [j]) -> d -> [k]

foldr (+).(.).map:
foldr (+) :: Num c => c -> [c] -> c
(.) :: (m -> n) -> (l -> m) -> l -> n
(.).map: (j -> k) -> (d -> [j]) -> d -> [k]
(m -> n) = Num c => c -> [c] -> c
m = c
n = [c] -> c
(l -> m) = (j -> k) -> (d -> [j]) -> d -> [k]
l = (j -> k)
m = (d -> [j]) -> d -> [k]
igualando m:
Num c = (d -> [j]) -> d -> [k]
foldr (+).(.).map :: Num (d -> [j]) -> d -> [k] => (j -> k) -> [(d -> [j]) -> d -> [k]] -> (d -> [j]) -> d -> [k]

——

(\x y z -> foldr z x y).map

(\x y z -> foldr z x y) :: b -> [a] -> (a -> b -> b) -> b
(.) :: (d -> e) -> (c -> d) -> c -> e
map :: (f -> g) -> [f] -> [g]

(d -> e) = b -> [a] -> (a -> b -> b) -> b
d = b
e = [a] -> (a -> b -> b) -> b

(c -> d) = (f -> g) -> [f] -> [g]
c = (f -> g)
d = [f] -> [g]

igualando d:
b = [f] -> [g]

(\x y z -> foldr z x y).map :: (f -> g) -> [a] -> (a -> ([f] -> [g]) -> [f] -> [g]) -> [f] -> [g]

——

map.((.) (foldr (++) (foldr (++) [] [[1], [2]])))

(foldr (++) [] [[1], [2]]):
foldr :: (a -> b -> b) -> b -> [a] -> b
(++) :: [c] -> [c] -> [c]
[] :: [d]
[[1],[2]] :: Num e => [[e]]
(a -> b -> b) = [c] -> [c] -> [c]
a = [c]
b = [c]
logo a = b = [c]
b = [d]
logo a = b = [c] = [d]
[a] = [[e]]
como a = b, [b] = [[e]], b = [e]
(foldr (++) [] [[1], [2]]) :: Num e => [e]

(foldr (++) (foldr (++) [] [[1], [2]])):
foldr :: (f -> g -> g) -> g -> [f] -> g
(++) :: [h] -> [h] -> [h]
(foldr (++) [] [[1], [2]]) :: Num e => [e]
(f -> g -> g) = [h] -> [h] -> [h]
f = [h]
g = [h]
g = [e]
logo f = [h] = g = [e]
(foldr (++) (foldr (++) [] [[1], [2]])) :: Num e => [[e]] -> [e]

((.) (foldr (++) (foldr (++) [] [[1], [2]]))):
(.) :: (j -> k) -> (i -> j) -> i -> k
(foldr (++) (foldr (++) [] [[1], [2]])) :: Num e => [[e]] -> [e]
(j -> k) = [[e]] -> [e]
j = [[e]]
k = [e]
((.) (foldr (++) (foldr (++) [] [[1], [2]]))) :: Num e => (i -> [[e]]) -> i -> [e]

map.((.) (foldr (++) (foldr (++) [] [[1], [2]]))):
map :: (l -> m) -> [l] -> [m]
(.) :: (o -> p) -> (n -> o) -> n -> p
((.) (foldr (++) (foldr (++) [] [[1], [2]]))) :: Num e => (i -> [[e]]) -> i -> [e]
(o -> p) = (l -> m) -> [l] -> [m]
o = (l -> m)
p = [l] -> [m]
(n -> o) = (i -> [[e]]) -> i -> [e]
n = (i -> [[e]])
o = i -> [e]
igualando o o:
l = i
m = [e]
map.((.) (foldr (++) (foldr (++) [] [[1], [2]]))) :: Num m => (i -> [[e]]) -> [i] -> [[e]]

——

(foldr).(.)$(!!)
Colocando parêntesis para ficar mais facil:
((foldr).(.))$(!!) 

(foldr).(.):
foldr :: (a -> b -> b) -> b -> [a] -> b
(.) :: (d -> e) -> (c -> d) -> c -> e
(.) :: (g -> h) -> (f -> g) -> f -> h
(d -> e) = (a -> b -> b) -> b -> [a] -> b
d = (a -> b -> b)
e = b -> [a] -> b
(c -> d) = (g -> h) -> (f -> g) -> f -> h
c = (g -> h)
d = (f -> g) -> f -> h
igualando d:
a = (f -> g)
b = h = f
c = (g -> f)
(foldr).(.) :: (g -> f) -> f -> [f -> g] -> f

(foldr).(.)$(!!):
(foldr).(.) :: (g -> f) -> f -> [f -> g] -> f
$ :: (i -> j) -> i -> j
(!!) :: [k] -> Int -> k
(i -> j) = (g -> f) -> f -> [f -> g] -> f
i = (g -> f)
j = f -> [f -> g] -> f
i = [k] -> Int -> k
igualando i:
g = [k]
f = Int -> k
(foldr).(.)$(!!) :: (Int -> k) -> [(Int -> k) -> [k]] -> Int -> k
