import Data.List

{-
Pregunta 1 (1,5 puntos) Define (y da el tipo más general) de la función agrupar que, dado n ::
Int y una lista s, devuelve una lista de listas, agrupando de n en n los elementos de s (y
despreciando los últimos si no llegan a ser n).
Debe cumplirse que n>0, en otro caso da un mensaje de error.
-}

agrupar :: Int -> [a] -> [[a]]
agrupar n s
  | n <= 0 = error "n debe ser mayor que 0"
  | otherwise = [take n (drop j s) | j <- [0, n .. length s - n]]

agru :: Int -> [a] -> [[a]]
agru n s
  | n <= 0 = error "n debe ser mayor que 0"
  | n > length s = []
  | otherwise = ys : agru n zs
  where
    (ys, zs) = splitAt n s

{-
Pregunta 2 (2,5 puntos) Dados los siguientes tipos:
-}

data Persona = Par Nombre Apellido deriving (Eq)

type Nombre = String

type Apellido = String

{-
a) Persona ha de ser una instancia de las clases Eq, Ord y Show de forma que la ordenación
sea por apellido (y luego por nombre) y se muestre (show) como en el ejemplo de abajo.
-}
instance Ord Persona where
  Par n1 a1 <= Par n2 a2 = (a1, n1) <= (a2, n2)

instance Show Persona where
  show (Par n a) = a ++ ", " ++ n

{-
b) Define (y da el tipo más general) de la función ordenarConMedia que, dada una lista de
personas con notas, devuelve la lista ordenada (por persona) junto con la media de sus notas
en el formato del siguiente ejemplo.
-}
ordenarConMedia :: [(Persona, Float)] -> IO ()
ordenarConMedia = putStrLn . pasarAstring . sort . juntarMedia

juntarMedia :: [(Persona, Float)] -> [(Persona, Float)]
juntarMedia [] = []
juntarMedia ((per1, t) : s) = (per1, (t + sum listaNotas) / num) : juntarMedia res
  where
    listaNotas = [nota | (persona, nota) <- s, persona == per1]
    num = 1 + longitud listaNotas
    res = [e1 | e1 <- s, (fst e1) /= per1]

longitud :: Num a => [a] -> Float
longitud [] = 0
longitud (_ : s) = 1 + longitud s

pasarAstring :: [(Persona, Float)] -> String
pasarAstring [] = ""
pasarAstring ((p, n) : s) = show p ++ "\t" ++ show n ++ "\n" ++ pasarAstring s

{-
Pregunta 3 (1,5 puntos)
Dado el siguiente tipo para árboles binarios:
 data Arbol a = Vacio | Nodo (Arbol a) a (Arbol a)
escribe el dato dibujado abajo (mi_arb) como elemento del tipo Arbol Int y define la
función nodosPorNiveles que dado un árbol ar devuelve la lista con todos los nodos de ar
recorridos éstos por niveles.
Ejemplo:
nodosPorNiveles mi_arb = [1,14,3,6,9,12,8] para el árbol binario siguiente:
-}

data Arbol a = Vacio | Nodo (Arbol a) a (Arbol a)

nodosPorNiveles :: Arbol a -> [a]
nodosPorNiveles ar = concat [nodosEnNivel i ar | i <- [1 .. n]]
  where
    n = altura ar

nodosEnNivel :: Int -> Arbol a -> [a]
nodosEnNivel _ Vacio = []
nodosEnNivel n (Nodo iz r de)
  | n == 1 = [r]
  | otherwise = nodosEnNivel (n - 1) iz ++ nodosEnNivel (n - 1) de

altura :: Arbol a -> Int
altura Vacio = 0
altura (Nodo iz r de) = 1 + max (altura iz) (altura de)

{-
mi_arb :: Arbol Int
mi_arb = Nodo (Nodo Vacio 6 Vacio ) 14 Vacio ) 1 (Nodo (Nodo Vacio 9 Vacio) 3 (Nodo (Nodo Vacio 8 Vacio ) 12 Vacio ))

-}