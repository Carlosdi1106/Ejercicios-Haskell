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