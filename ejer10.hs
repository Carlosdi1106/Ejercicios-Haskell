module EjemplosTema10 where

import Data.Char -- para poder usar la funcion toUpper

leer :: Int -> IO String
leer 0 = return []
leer n = getChar >>= q
  where
    q c = leer (n - 1) >>= r
      where
        r cs = return (c : cs)

-- 1.1 Haz otros dos programas equivalentes a leer que utilicen la notación do

leer1 :: Int -> IO String
leer1 0 = return []
leer1 n = do
  c <- getChar
  cs <- leer (n - 1)
  return (c : cs)

{-leer2 :: Int -> IO String
leer2 0 =return []
leer2 n = do
            c <- getChar
            cs <- leer (n-1)
            return concat (c++cs)
            -}

leerLinea :: IO String
leerLinea = getChar >>= q
  where
    q c =
      if c == '\n'
        then return []
        else leerLinea >>= f
      where
        f cs = return (c : cs)

leerCaracter :: IO Char
leerCaracter = do
  c <- getChar
  if c == ' '
    then return ' '
    else do return (toUpper c)

leeEntero :: IO Int
{-leeEntero = getChar >>= q
 where
   q c = if (read c) > 0 then (read c)
   else error "No es un entero"-}

leeEntero = do
  e <- getChar
  if e == ' '
    then return error "No es un entero"
    else do return (read e)
