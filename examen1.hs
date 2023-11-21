-- 1. Da soluciones alternativas para las siguientes f1 y f2:

-- f1 s = map (+3) (filter even s)
-- f2 s = filter even (map (+3) s)

-- Da definiciones alternativas usando listas intensionales (no debe aparecer ni filter ni map).
-- Soluciones alternativas: f1' y f2'

f1' s = [x + 3 | x <- s, even x]

f2' s = [x | x <- [y + 3 | y <- s], even x]

f2b' s = [x + 3 | x <- s, even (x + 3)]

-- 2.Define una funcion que devuleva la lista mas corta de una lista de listas.
-- Si hay varias listas de la misma longitud, cualquiera de ellas sirve como resultado.
-- (a) recursividad usando los patrones de listas
-- (b) usando foldr1

pla = [[1 .. 3], [4 .. 10], [1]]

plb = drop 3 pla

shortest :: [[a]] -> [a]
-- shortest [] = [] -- [[]]???
shortest (xs : []) = xs
shortest (xs : ys : zss)
  | length xs < length ys = shortest (xs : zss)
  | otherwise = shortest (ys : zss)

shortestF :: [[a]] -> [a]
shortestF (xs : xss) = foldr1 myShortestLength (xs : xss)

myShortestLength xs [] = xs
myShortestLength xs ys
  | length xs < length ys = xs
  | otherwise = ys

-- 3. La funcion paresSeg xs = zip (init xs) (tail xs) obtene la lista de pares de elementos consecutivos de la lista xs, Por ejemplo:
-- paresSeg [1,2,2,3,2,4,4,5] = [(1,2),(2,2),(2,3),(3,2),(2,4),(4,4),(4,5)]
-- Utilizando la funcion paresSeg y una lista intesional, define una funcion comprimida que elimina las repeticiones consecutivas. Por ejemplo:
-- comprimir [1,2,2,3,2,4,4,5] = [1,2,3,2,4,5]
ps1 = [1, 2, 2, 3, 2, 4, 4, 5]

ps2 = [1, 1, 2, 2, 2, 2, 3, 2, 4, 4, 5]

paresSeg xs = zip (init xs) (tail xs)

comprimir xs = [x | (x, y) <- paresSeg xs, (x /= y)] ++ [last xs]

-- 4.- Define una funcionalidad lo mas general posible denominada esCreciente que determine si la lista es o no monotono creciente:
-- Resulevela de varias formas, una debe ser con listas intensionales, otra zipWith y and

-- esCreciente :: [a] -> Bool
-- esCreciente [] = True
-- esCreciente [x] = True
-- esCreciente (x:y:xs) = (x<y) && esCreciente (y:xs)

esCreciente :: Ord a => [a] -> Bool
esCreciente xs = and (zipWith (<=) xs (tail xs))

-- 5. Define, utilizando listas intensionales, la funcion mapIf que, dada un predicado p, una funcion unaria f y una lista xs, produzca la lista que resulta de aplicar f a todos los elementos de xs que cumplan p y mantener ls elementos que no cumplen p tal como aparecen en xs. Por ejemplo:

--

mapIf p f [] = []
mapIf p f (x : xs)
  | p x = f x : mapIf p f xs
  | otherwise = x : mapIf p f xs

-- Con listas intensionales
mapIf' p f xs = [if p x then f x else x | x <- xs]