------------------------------------------------------------------
--  PRACTICA: CALENDARIO           --               PF  2023-24

--  Nombre: Carlos Diez
------------------------------------------------------------------

-- Llamada principal es:  printCalendario c n
-- donde c = columnas (3 o 4) y
--       n = año cuyo calendario deseamos imprimir
------------------------------------------------------------------

module Calendario where

import Data.List

type Dibujo = [Linea] -- cada dibujo es una lista de lineas

type Linea = [Char] -- cada linea es una lista de caracteres

type Year = Int

type Columna = Int -- es 3 o 4

-- Para imprimir un dibujo en pantalla:
printDibujo :: Dibujo -> IO ()
printDibujo dib = do
  putStr "\n" -- putStr es la función que imprime un String
  (putStr . concat . map (++ "\n")) dib

-- Imprime, con un numero de columnas, el calendario de un año:
printCalendario :: Columna -> Year -> IO ()
printCalendario c a = printDibujo (calendario c a)

-- Dibujo de un calendario (en c columnas) de un año dado:
calendario :: Columna -> Year -> Dibujo
calendario c = bloque c . map dibujomes . meses

---------------------------------------------------
--  Define las siguientes funciones sobre dibujos:
---------------------------------------------------

dibEsCorrecto :: Dibujo -> Bool
-- comprueba que las lineas de un dibujo tienen igual longitud,
-- debe dar un mensaje de error si el dibujo es vacío ([]).
dibEsCorrecto [] = error "El dibujo es vacio"
dibEsCorrecto (x : xs) = auxDibEsCorrecto xs (length x)
  where
    auxDibEsCorrecto [] _ = True
    auxDibEsCorrecto (x : xs) l
      | (length x) == l = auxDibEsCorrecto xs l
      | otherwise = False

listaDibCorrectos :: [Dibujo] -> Bool
-- comprueba que los dibujos de la lista dada son correctos y
-- ademas tienen todos las mismas dimensiones.
listaDibCorrectos [] = True
listaDibCorrectos xs = auxListaDibCorrecto (tail xs) (ancho (head xs)) (alto (head xs)) (dibEsCorrecto (head xs))
  where
    auxListaDibCorrecto [] _ _ True = True
    auxListaDibCorrecto xs an al b
      | (ancho (head xs)) == an && (alto (head xs)) == al && b == True = auxListaDibCorrecto (tail xs) an al (dibEsCorrecto (head xs))
      | otherwise = False

alto :: Dibujo -> Int
-- Pre: dib es un dibujo correcto.
-- alto dib da la altura de dib.
alto xs = length xs

ancho :: Dibujo -> Int
-- Pre: dib es un dibujo correcto.
-- ancho dib da la anchura de dib.
ancho (x : xs) = length x

sobre :: Dibujo -> Dibujo -> Dibujo
-- Precondicion: los dibujos d1 y d2 tienen la misma anchura.
-- sobre d1 d2 pone el dibujo d1 sobre el dibujo d2.
sobre xs ys = xs ++ ys

alLado :: Dibujo -> Dibujo -> Dibujo
-- Precondicion: los dibujos d1 y d2 tienen la misma altura.
-- alLado d1 d2 da un dibujo con d1 a la izquierda de d2.
alLado [] ys = ys
alLado xs [] = xs
alLado xs ys
  | xs /= [] = [(head xs) ++ (head ys)] ++ alLado (tail xs) (tail ys)
  | otherwise = []

apilar :: [Dibujo] -> Dibujo
-- apila s da el dibujo obtenido apilando todos los elementos de s
--         (el primero de s queda en la cima de la pila).
-- Si s no es una lista de dibujos correctos debe dar error.
apilar xs
  | listaDibCorrectos xs = apilao xs
  | otherwise = error "Esta incorrecto la lista"
  where
    apilao xs = foldl sobre [] xs

extender :: [Dibujo] -> Dibujo
-- extiende s da el dibujo obtenido al extender todos los elementos
--            de s (el primero de s queda el más a la izquierda).
-- Si s no es una lista de dibujos correctos debe dar error.
extender xs
  | listaDibCorrectos xs = extendio xs
  | otherwise = error "Esta incorrecto la lista"
  where
    extendio xs = foldr alLado [] xs

dibBlanco :: (Int, Int) -> Dibujo
-- Precondicion: al>0 && an>0.
-- dibBlanco (al,an) devuelve el dibujo de caracteres blancos con
--                   altura al y anchura an

dibBlanco (al, an) = [lineaBlanca an | n <- [1 .. al]]
  where
    lineaBlanca an = replicate an ' '

bloque :: Int -> [Dibujo] -> Dibujo
-- bloque n lisDib es el dibujo formado al agrupar de n en n los
--               dibujos de lisDib, extender cada sublista
--               y luego apilar los resultados.
-- Debido a que existe la opcion de dibBlanco, he agregado la posibilidad de que las columnas sean 5 o mas.
bloque n lisDib = apilar (map extender (agrupar n lisDib))
  where
    agrupar _ [] = []
    agrupar n xs
      | length (take n xs) == n = take n xs : agrupar n (drop n xs)
      | otherwise = [take n xs ++ replicate (n - length (take n xs)) (dibBlanco (10, 25))]

-- otras funciones auxiliares sobre dibujos que se necesiten:

------------------------------------------------------------------
-- Define constantes y funciones para calcular y dibujar los meses
------------------------------------------------------------------

meses :: Year -> [(String, Year, Int, Int)]
-- meses n devuelve una lista de 12 elementos con los datos
--         relevantes de cada uno de los meses del año n:
--         (nombre_mes, n, primer_día_mes, longitud_mes)
meses n = zip4 nombresmeses (repeat n) (pdias n) (longitudDelMes n)

dibujomes :: (String, Year, Int, Int) -> Dibujo
-- dibujomes (nm,a,pd,lm) devuelve un dibujo de dimensiones 10x25
-- formado por el titulo y la tabla del mes de nombre nm y año a.
-- Necesita como parámetros: pd=primer dia y lm=longitud del mes.
dibujomes (m, a, b, c) = dibujomesParteFinal (fechas b c) (dibujomesInicio m a) 1

ene1 :: Year -> Int
ene1 a = mod (a + div (a - 1) 4 - div (a - 1) 100 + div (a - 1) 400) 7

-- ene1 a devuelve el dia de la semana del 1 de enero del año a
--        siendo 1=lunes, 2=martes, ..., 6=sabado, 0=domingo

pdias :: Year -> [Int]
-- pdias a  devuelve una lista con 12 dias que son los dias de la
--          semana en que comienza cada mes del año a siendo
--          1=lunes, 2=martes, ..., 6=sabado y 7=domingo
-- Ejemplo: pdias 2019 es [2,5,5,1,3,6,1,4,7,2,5,7]

pdias a = listaa (ene1 a) (longitudDelMes a)

nombresmeses :: [String]
nombresmeses =
  [ "Enero",
    "Febrero",
    "Marzo",
    "Abril",
    "Mayo",
    "Junio",
    "Julio",
    "Agosto",
    "Septiembre",
    "Octubre",
    "Noviembre",
    "Diciembre"
  ]

fechas :: Int -> Int -> [Dibujo]
-- fechas pd lm da una lista de 42 dibujos de 1*3 (alguno blanco)
--              con los dias de un mes cuyo primer dia de semana
--              es pd y cuya longitud de mes es lm
fechas pd lm = [[dias x] | x <- [1 .. 42]] ---falta añadir \n
  where
    dias x
      | dia x <= 0 || dia x > lm = "   "
      | otherwise = " " ++ espacio (dia x)
    espacio d
      | length (show d) == 1 = " " ++ show d
      | otherwise = show d
    dia x = x - pd + 1

-- otras funciones que se necesiten:

dibujomesInicio :: String -> Year -> [String]
dibujomesInicio f a = [" " ++ f ++ " " ++ show a ++ replicate (25 - (length f + length (show a) + 2)) ' '] ++ [replicate 25 ' '] ++ [" Lu Ma Mi Ju Vi Sa Do    "]

dibujomesParteFinal :: [Dibujo] -> Dibujo -> Int -> Dibujo
dibujomesParteFinal xs ys cont
  | cont <= 6 = dibujomesParteFinal xs (ys ++ dibujomesFinalAux xs cont) (cont + 1)
  | otherwise = ys ++ [replicate 25 ' ']

dibujomesFinalAux :: [Dibujo] -> Int -> Dibujo
dibujomesFinalAux xs ind = [concat (concat (take 7 (drop (tirar ind) xs))) ++ replicate 4 ' ']
  where
    tirar ind = 7 * (ind - 1)

longitudDelMes :: Year -> [Int]
longitudDelMes a = [31, (feb a), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

feb :: Year -> Int
feb a
  | esBisiesto a = 29
  | otherwise = 28

esBisiesto :: Year -> Bool
esBisiesto a
  | (a `mod` 4 == 0 && a `mod` 100 /= 0) || (a `mod` 400 == 0) = True
  | otherwise = False

listaa :: Year -> [Int] -> [Int]
listaa _ [] = []
listaa x (y : ys)
  | x < 7 = [x] ++ listaa (x + y) ys
  | x > 7 = listaa (x - 7) (y : ys)
  | otherwise = [x] ++ listaa (x + y) ys

--------------------------------------------------------------------
