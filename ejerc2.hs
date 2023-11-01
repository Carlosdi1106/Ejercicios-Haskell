-- 1. Define una función quitaUno que quite de una lista dada la primera aparición (si existe alguna) de un elemento dado. Ejemplo: quitaUno 5 [1,3,5,7,3,5,8] = [1,3,7,3,5,8]

quitaUno :: Eq a => a -> [a] -> [a]
quitaUno _ [] = []
quitaUno x (y : ys)
  | x == y = ys
  | otherwise = [y] ++ quitaUno x ys

-- 2. Define una función quitaRep que dada una lista elimine los elementos repetidos. Ejemplo: quitaRep [1,3,5,7,3,5,8] = [1,3,5,7,8]

quitaRep :: Eq a => [a] -> [a]
quitaRep [] = []
quitaRep (x : xs) = x : quitaRep (filter (/= x) xs)

-- 3. Usando quitaUno, define una función dif que realice la diferencia de dos listas. Ejemplo: dif [1,2,3,7,4,2,8,4] [3,5,2,4,2] = [1,7,8,4]

dif :: Eq a => [a] -> [a] -> [a]
dif [] _ = []
dif xs [] = xs
dif xs (y : ys) = dif (quitaUno y xs) ys

-- 4. Usando dif, define una función perm que decida si una lista es permutación de otra. Ejemplos: perm “abc” “acb” = True perm [1,2,2,4] [2,4,1] = False

perm :: Eq a => [a] -> [a] -> Bool
perm xs ys = null (dif xs ys) && null (dif ys xs)

-- 5. Usando perm y filter, define una función sonPermDe1 que, dada una lista de listas xss , obtenga la sublista de xss formada por aquellos elementos que son permutaciones de la cabeza de xss. Ejemplo: sonpermde1 [ “abc”, “ac”, “bca”, “bbc”, “cab”] = [ “abc”, “bca”, “cab”]

sonPermDe1 :: Eq a => [[a]] -> [[a]]
sonPermDe1 [[]] = [[]]
sonPermDe1 xss = filter (perm (head xss)) xss

-- 6. Usando alguna versión de fold, define una función aDecimal que convierta una lista de dígitos en el número decimal que representa. Define su función inversa aDigitos. Ejemplos: aDecimal [9,6,3,8] = 9638 aDigitos 9638 = [9,6,3,8]

aDecimal :: [Int] -> Int
aDecimal digits = foldl (\acc digit -> acc * 10 + digit) 0 digits

aDecimal2 :: [Int] -> Int
aDecimal2 = foldl1 f
  where
    f x y = 10 * x + y

aDigitos :: Int -> [Int]
aDigitos n = foldr (\d acc -> d : acc) [] (map (\c -> read [c]) (show n))

aDigitos2 :: Integer -> [Integer]
aDigitos2 0 = []
aDigitos2 n
  | n <= 0 && n <= 9 = [n]
  | otherwise = aDigitos2 (div n 10) ++ [mod n 10]

-- 7. Define las funciones decimalAbinario y binarioAdecimal para transformar un número decimal a binario y viceversa. Ejemplos: decimalAbinario 13 = 1101 binarioAdecimal 1101 = 13

decimalAbinario :: Integer -> Integer
decimalAbinario n
  | n == 0 = 0
  | otherwise = (decimalAbinario (div n 2)) * 10 + (mod n 2)

binarioAdecimal :: Integer -> Integer
binarioAdecimal n = foldl (\acc bit -> acc * 2 + bit) 0 (aDigitos2 n)

-- 8. Define un predicado que decida si una lista está ordenada. Ejemplos: ordenada “ayu” = False ordenada [1,3,7,12] = True

ordenada :: Ord a => [a] -> Bool
ordenada [] = True
ordenada [_] = True
ordenada (x : xs) = x <= (head xs) && ordenada xs

-- 9. Usando takeWhile y dropWhile, define la función palabras que dada una frase (string) obtenga la lista de palabras (lista de strings) de la frase. Ejemplo: palabras “ Estoy en el laboratorio” = [“Estoy”, “en”, “el”, “laboratorio”]

palabras :: String -> [String]
palabras [] = []
palabras frase = takeWhile (/= ' ') frase : palabras (dropWhile (== ' ') (dropWhile (/= ' ') frase))

-- 10. Define la función posiciones que devuelva las posiciones de un elemento dado en una lista dada. Ejemplo: posiciones 8 [8,9,5,8,2] = [0,3]

posiciones :: Eq a => a -> [a] -> [Int]
posiciones x xs = [i | (i, y) <- zip [0 ..] xs, y == x]

-- 11. Define una función paraTodo que dado un predicado y una lista decida si todos sus elementos satisfacen el predicado, y una función existe que dado un predicado y una lista decida si alguno de sus elementos satisface el predicado. Compáralas con las predefinidas: all, any :: (α -> Bool) -> [α] -> Bool

paraTodo :: (a -> Bool) -> [a] -> Bool
paraTodo _ [] = True
paraTodo p (x : xs)
  | p x = paraTodo p xs
  | otherwise = False

existe :: (a -> Bool) -> [a] -> Bool
existe _ [] = False
existe p (x : xs)
  | p x = True
  | otherwise = existe p xs

-- 12. Define una función permutar que devuelva todas las permutaciones de una lista dada. Ejemplo: permutar [2,8,3] = [[2,8,3],[8,2,3],[8,3,2],[2,3,8],[3,2,8],[3,8,2]]

permutar :: Eq a => [a] -> [[a]]
permutar [] = [[]]
permutar xs = [x : ys | x <- xs, ys <- permutar (quitar x xs)]
  where
    quitar y = filter (/= y)

-- 13. Una lista es una sublista de otra si los elementos de la primera aparecen en la segunda en el mismo orden. Una lista es una subsecuencia de otra si aparece en ella como secuencia de elementos contiguos. Define las funciones sublista y subsecuencia. Ejemplos: sublista “palma” “tapa la mesa” = True subsecuencia “palma” “tapa la mesa” = False

sublista :: Eq a => [a] -> [a] -> Bool
sublista [] _ = True
sublista _ [] = False
sublista (x : xs) (y : ys)
  | x == y = sublista xs ys
  | otherwise = sublista (x : xs) ys

subsecuencia :: Eq a => [a] -> [a] -> Bool
subsecuencia [] _ = True
subsecuencia _ [] = False
subsecuencia (x : xs) (y : ys)
  | x == y = subsecuenciaRestante xs ys
  | otherwise = subsecuencia (x : xs) ys

subsecuenciaRestante :: Eq a => [a] -> [a] -> Bool
subsecuenciaRestante [] _ = True
subsecuenciaRestante _ [] = False
subsecuenciaRestante (x : xs) (y : ys)
  | x == y = subsecuenciaRestante xs ys
  | otherwise = False

-- 14. Evalúa la siguiente expresión, descrita mediante una lista intensional: [j | k <− [1,-1, 2, -2], k>0, j <− [1.. k]]

-- El resultado seria de [1,1,2]

-- 15. Dado un string, define una función diag que devuelva cada carácter del string en una línea y en diagonal. Ejemplo:
{--? diag “haskell”
h
 a
  s
   k
    e
     l
      l
--}
diag :: String -> IO ()
diag frase = putStr (concat ([replicate (indi - 1) ' ' ++ [car] ++ "\n" | (car, indi) <- zip frase [1 ..]]))

-- 16. Define una función repLong que dado un string devuelva tantas copias del string como su longitud y una en cada línea. Ejemplo:
{--? repLong “hugs”
      hugs
      hugs
      hugs
      hugs
--}

repLong :: String -> IO ()
repLong str = putStr (concat (replicate (length str) (str ++ "\n")))

-- Los siguientes problemas deben resolverse empleando LISTAS INTENSIONALES
-- 17. Da definiciones alternativas a las funciones de los ejercicios 10, 11 y 12 utilizando listas intensionales.

-- 18. Usar elem y listas intesionales para definir la función intersec:: [α]->[α]->[α], tal que intersec xs ys obtenga la lista de todos los elementos de xs que también están en ys.

intersec :: Eq a => [a] -> [a] -> [a]
intersec xs ys = [x | x <- xs, elem x ys]

-- 19. Usar length para definir la función numVeces que calcule el número de apariciones de un elemento dado en una lista dada. Ejemplo: numVeces 8 [8,9,5,8,2] -> 2.

numVeces :: Eq a => a -> [a] -> Int
numVeces n xs = length [x | x <- xs, n == x]

-- 20. Define una función que, dadas dos listas, decida si en ambas aparen exactamente los mismos elementos (el número de apariciones es irrelevante).

mismaLista :: Eq a => [a] -> [a] -> Bool
mismaLista xs ys = perm (quitaRep xs) (quitaRep ys)

-- 21. Usar zip para definir una función que calcule las posiciones de un elemento dado en una lista dada. Ejemplo: posiciones 8 [8,9,5,8,2] -> [0,3].

-- posiciones :: Eq a => a -> [a] -> [Int]
-- posiciones x xs = [i | (i, y) <- zip [0 ..] xs, y == x]

-- 22. Usar map, filter y zip para definir una función que, dada una lista, obtiene la lista de todos sus elementos cuya posición es impar (recuerda que la primera posición es 0).

elementosPosicionImpar :: [a] -> [a]
elementosPosicionImpar xs = map siguienteElemento (filter (\(i, _) -> inpar i) (zip [0 ..] xs))

siguienteElemento :: (a, b) -> b
siguienteElemento (_, b) = b

inpar :: Int -> Bool
inpar n = n `mod` 2 /= 0

-- 23. Usar and y zip para definir una función que decida si todos los elementos de una lista son iguales.

sonTodosIguales :: Eq a => [a] -> Bool
sonTodosIguales xs = and [x == y | (x, y) <- zip xs (tail xs)]
