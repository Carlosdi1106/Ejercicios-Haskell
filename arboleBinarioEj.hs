data Arbin a = Hoja a | Unir (Arbin a) (Arbin a)

instance Show a => Show (Arbin a) where
  show = mostrar

mostrar :: Show a => Arbin a -> String
mostrar ar = ver ar 0

blancos :: Int -> String
blancos n = [' ' | i <- [1 .. n]]

ver :: Show a => Arbin a -> Int -> String
ver (Hoja x) n = blancos n ++ show x ++ "\n"
-- 5 blancos por cada n = profundidad
ver (Unir ai ad) n = ver ad (n + 5) ++ blancos n ++ "*" ++ "\n" ++ ver ai (n + 5)

-- Datos de prueba:
nodo1 :: Arbin Int
nodo1 = (Hoja 1)

nodo2 :: Arbin Int
nodo2 = (Hoja 2)

arb1 :: (Arbin Int)
arb1 = (Unir nodo1 nodo2)

nodo3 :: Arbin Int
nodo3 = (Hoja 3)

nodo4 :: Arbin Int
nodo4 = (Hoja 4)

arb2 :: (Arbin Int)
arb2 = (Unir nodo3 nodo4)

arbTodo :: (Arbin Int)
arbTodo = (Unir arb1 arb2)

-- Arbin Arbus --Árboles de búsqueda
data Arbus a = Vac | Nod (Arbus a) a (Arbus a) deriving (Show)

-- Datos de prueba:

arbus1 :: (Arbus Int)
arbus1 = Nod arbus2 4 arbus3

arbus2 :: (Arbus Int)
arbus2 = Nod Vac 2 Vac

arbus3 :: (Arbus Int)
arbus3 = Nod Vac 6 Vac

aplanar :: Arbus a -> [a]
aplanar Vac = []
aplanar (Nod ai r ad) = aplanar ai ++ [r] ++ aplanar ad

estaOrd :: Ord a => Arbus a -> Bool
estaOrd = ordenada . aplanar

ordenada :: Ord a => [a] -> Bool
ordenada [] = True
ordenada [_] = True
ordenada (x : y : resto)
  | x <= y = ordenada (y : resto)
  | otherwise = False

esta :: Ord a => a -> Arbus a -> Bool
esta x Vac = False
esta x (Nod ai r ad)
  | x < r = esta x ai
  | x == r = True
  | x > r = esta x ad

meter :: Ord a => a -> Arbus a -> Arbus a
meter x Vac = Nod Vac x Vac
meter x (Nod ai r ad)
  | x < r = Nod (meter x ai) r ad
  | x == r = Nod ai r ad
  | x > r = Nod ai r (meter x ad)

borrar :: Ord a => a -> Arbus a -> Arbus a
borrar x Vac = Vac
borrar x (Nod ai r ad)
  | x < r = Nod (borrar x ai) r ad
  | x == r = une ai ad
  | x > r = Nod ai r (borrar x ad)

une :: Ord a => Arbus a -> Arbus a -> Arbus a
une ai ad = reconstruir (aplanar ai ++ aplanar ad)

-- arb :: Ord a => [a] -> Arbus a
-- arb [] = Vac
-- arb (x : y : z : resto) = Nod (Nod x y z)

reconstruir :: Ord a => [a] -> Arbus a
reconstruir [] = Vac
reconstruir xs = Nod (reconstruir ai) r (reconstruir ad)
  where
    len = length xs
    (ai, r : ad) = splitAt (len `div` 2) xs
