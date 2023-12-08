{--data Persona = Per String String Int String String deriving (Show)

nombre :: Persona -> String
nombre (Per nom _ _ _ _) = nom

apellido :: Persona -> String
apellido (Per _ ap _ _ _) = ap

edad :: Persona -> Int
edad (Per _ _ ed _ _) = ed

numTelefono :: Persona -> String
numTelefono (Per _ _ _ num _) = num

ciudad :: Persona -> String
ciudad (Per _ _ _ _ ciu) = ciu-}

data Persona2 = Per2
  { nombre2 :: String,
    apellido2 :: String,
    edad2 :: Int,
    numTelefono2 :: String,
    ciudad2 :: String
  }
  deriving (Show)

-- type Fecha :: (Int, Int, Int)

data Persona = Per String String Int String String deriving (Show)

data Arbin a = Hoja a | Unir (Arbin a) (Arbin a)

ab1 :: Arbin Int
ab1 = Unir (Hoja 8) (Unir (Hoja 6) (Unir (Hoja 1) (Hoja 7)))

type Interprete = String

type Titulo = String

type NcopiasVendidas = Int

data Cancion = Can (Titulo, Interprete, NcopiasVendidas) deriving (Show)

instance Eq Cancion where
  Can (t, i, _) == Can (t1, i1, _) = (t == t1 && i == i1)

type Catalogo = [Cancion]

-- implements Eq where

normalizar :: Catalogo -> Catalogo
-- Quitar las canciones repetidas y sumas su correspondiente NcopiasVendidas
normalizar [] = []
normalizar [c1] = [c1]
normalizar (x : xs) = anadir x : normalizar (filter (/= x) xs)
  where
    anadir (Can (t, i, _)) = Can (t, i, apariciones x (x : xs))
    apariciones _ [] = 0
    apariciones c (z : zs)
      | c == z = nCopiasVendidas z + apariciones c zs
      | otherwise = apariciones c zs

nCopiasVendidas (Can (_, _, x)) = x

quitar :: Cancion -> Catalogo -> Catalogo
quitar _ [] = []
quitar x (z : zs)
  | x == z = quitar x zs
  | otherwise = z : quitar x zs

normalizar2 :: Catalogo -> Catalogo
-- Version de la Profesora
normalizar2 [] = []
normalizar2 (Can (t, i, cv) : resto) = Can (t, i, cv + cvTotal) : normalizar l2
  where
    cvTotal = (sum . map cantidad) l1
    -- sum.map recoje todos los elementos de cantidad y los suma.
    l1 = filter (== Can (t, i, cv)) resto
    l2 = filter (/= Can (t, i, cv)) resto

cantidad :: Cancion -> NcopiasVendidas
cantidad (Can (t, i, cv)) = cv

cata :: Catalogo
cata = [Can ("A", "B", 2), Can ("C", "D", 2), Can ("E", "F", 2), Can ("G", "H", 2), Can ("I", "J", 2), Can ("K", "L", 2), Can ("A", "B", 2), Can ("O", "P", 2), Can ("Q", "R", 2), Can ("S", "T", 2)]

-- normalizar xs
-- Hacer una lista de que vaya recojiendo las canciones y cuando encuentre una cancion repetida, sumarle el monto y borrar la repe. Con recursion ir creando una nueva lista donde solo existe la primera version y luego las repetidas dejarlas fuera.
-- listaProc c cat =
-- tituloMasVendido :: Catalogo -> Titulo
-- Titulos con mas copias vendidas
