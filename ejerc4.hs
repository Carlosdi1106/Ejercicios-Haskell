-- 1. Da el tipo más general posible de las siguientes funciones. Comprueba después qué responde el sistema como tipo inferido.
-- constante x y = x (predefinida const)
-- constante x _ = x

-- subst f g x = f x (g x)

-- aplicar f x = f x

-- fliparg f x y = f y x (predefinida flip)

-- cuadrado x = x * x

-- rara f g = g ( f g)

-- masRara f = f f

-- 2. Dada la siguiente definición para la función casos, pregunta :t casos y compáralo con el tipo de la función predefinida either.

-- casos (f, g) (Left x) = f x
-- casos :: (t1 -> t2, b1) -> Either t1 b2 -> t2
-- casos (f, g) (Right y) = g y
-- casos :: (a1, t1 -> t2) -> Either a2 t1 -> t2

-- 7. Dados los siguientes tipos de datos:
type Direccion = (Persona, Dir, Ciudad)

type Nombre = String

type Apellido = String

type Ciudad = String

data Persona = Per Nombre Apellido

data Dir = Calle String Int | Casa String

--  Define dos elementos dirJon y dirMiren de tipo Direccion (con los datos dados en el ejemplo).
dirJon, dirMiren :: Direccion
dirJon = (Per "Jon" "Prieto", Casa "Enea", "Orio")
dirMiren = (Per "Miren" "Artola", Calle "Aldamar" 15, "Donostia")

--  Define una función escribir :: [Direccion] -> IO( ) que imprima en pantalla una lista de direcciones de personas con el siguiente formato:
-- ? escribir [dirJon, dirMiren]
-- Jon Prieto
-- casa Enea
-- Orio

-- Miren Artola
-- c/ Aldamar, 15
-- Donostia
escribir :: [Direccion] -> IO ()
escribir = putStr . listadirAstring

listadirAstring :: [Direccion] -> String
listadirAstring [] = " "
listadirAstring (x : xs) = direccionAstring x ++ "\n" ++ listadirAstring xs

direccionAstring :: Direccion -> String
direccionAstring (Per nom ap, dir, ciudad) = nom ++ " " ++ ap ++ "\n" ++ dirAstring dir ++ "\n" ++ ciudad ++ "\n"
  where
    dirAstring (Calle calle num) = "c/ " ++ calle ++ ", " ++ show num
    dirAstring (Casa casa) = "casa " ++ casa

{-
  Para quitar la ultima funcion de dirAstring, se puede cambiar el data:
  data Dir = ....
    implements Show where
        show =

    escribir (x:xs) = show x ++ escribir xs

    escribir xs = (putStr.unlines)([[x1++' '++x2,'casa '++y1,z] | (Per x1 x2, casa y1, z) <- xs] ++ [[x1 ++ ' '++x2, 'c/ '++y1++' '++show y2,z]|(Per x1 x2, calle y1 y2, z) <- xs])
-}

-- 8. Dado el siguiente tipo algebraico data Elemento = E String Int deriving Show que permite representar palabras con un contador asociado a cada una.

data Elemento = E String Int deriving (Show)

-- -> Define las instancias de Eq y Ord necesarias para que los objetos del tipo Elemento puedan compararse y ordenarse con respecto a su componente de tipo String (sin que el contador afecte al orden).

instance Eq Elemento where (E e1 _) == (E e2 _) = e1 == e2

instance Ord Elemento where (E e1 _) <= (E e2 _) = e1 <= e2

-- -> Define una función dosMayores :: [Elemento]  (Elemento, Elemento) que obtenga el par formado por los dos elementos mayores de la lista dada, según el orden definido arriba. La función dosMayores debe recorrer la lista una única vez
-- dosMayores [E "a" 2, E "b" 3, E "c" 5, E "d" 7] = (E "c" 5,E "d" 7)
-- dosMayores [E "a" 2, E "b" 3, E "c" 5, E "a" 7] = (E "b" 3,E "c" 5)
-- El primer Elemento es el menor de los dos.

dosMayores :: [Elemento] -> (Elemento, Elemento)
dosMayores [] = error "No hay lista"
dosMayores [x] = error "Se necesita al menos dos elementos"
dosMayores (x : y : xs)
  | x < y = dosM xs (x, y)
  | otherwise = dosM xs (y, x)

dosM :: [Elemento] -> (Elemento, Elemento) -> (Elemento, Elemento)
dosM [] (x, y) = (x, y)
dosM (z : zs) (x, y)
  | y <= z = dosM zs (y, z)
  | x < z = dosM zs (z, y)
  | otherwise = dosM zs (x, y)

-- 9. Define el tipo algebraico data Racional = Int :/ Int como:
data Racional = Int :/ Int

--  Instancia de la clase Eq de forma que la igualdad coincida con su definición matemática en los números racionales. Ejemplo: 1:/2 es igual a 2:/4
instance Eq Racional where (x :/ y) == (p :/ q) = (x * q == y * p)

--  Instancia de la clase Ord de forma que el orden coincida con su definición matemática en los números racionales. Ejemplo: 1:/4 es menor que 3:/5
-- Nota: Basta definir el método (<=) y para el resto de métodos considerar la definición por defecto.
instance Ord Racional where (x :/ y) <= (p :/ q) = (x * q <= y * p)

--  Instancia de la clase Show de manera que se muestren los elementos lo más reducidos posible. Ejemplo: 15:/6 se muestra como 5/2 y 8:/2 como 4.
instance Show Racional where
  show (x :/ y)
    | denominador == 1 = show numerador
    | otherwise = show numerador ++ "/" ++ show denominador
    where
      z = mcd x y
      numerador = div x y
      denominador = div y z

-- mcd da el maximo comun divisor de dos numeros:
mcd :: Int -> Int -> Int
mcd a b
  | a == 0 && b == 0 = error "Los dos son cero"
  | otherwise = mcd_pos (abs a) (abs b)

-- mcd con dos numeros positivos.
mcd_pos :: Int -> Int -> Int
mcd_pos a b
  | b == 0 = a
  | otherwise = mcd_pos b (mod a b)
