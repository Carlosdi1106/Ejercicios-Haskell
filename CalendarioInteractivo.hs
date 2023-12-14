-----------------------------------------------------------------
-- PRACTICA CALENDARIO INTERACTIVO     2023-24
-----------------------------------------------------------------

-- La llamada principal debe ser main

module CalendarioInteractivo where

import Calendario -- importar el modulo de la Practica 1

-------------------------------------

-- Función para validar el año
validarYear :: Int -> Bool
validarYear a = a > 0 && a < 2500

-- Función para validar el número de columnas
validarColumnas :: Int -> Bool
validarColumnas c = c == 3 || c == 4

-- Función principal para obtener el año del usuario
pedirYear :: IO Int
pedirYear = do
  putStr "Ingrese el año para el calendario: "
  a <- getLine
  if todosSonDigitos a && validarYear (read a)
    then return (read a)
    else do
      putStrLn "Año no válido. Debe ser un año mayor que 0 y menor que 2500."
      pedirYear

-- Función principal para obtener el número de columnas del usuario
pedirColumnas :: IO Int
pedirColumnas = do
  putStr "Ingrese el número de columnas (3 ó 4) para el calendario: "
  c <- getLine
  if todosSonDigitos c && validarColumnas (read c)
    then return (read c)
    else do
      putStrLn "Número de columnas no válido. Debe ser 3 ó 4."
      pedirColumnas

-- Función para obtener la confirmacion de si el caracter es digito o no.
esDigito :: Char -> Bool
esDigito c = c >= '0' && c <= '9'

-- Función para validar si todos los caracteres son digitos.
todosSonDigitos :: [Char] -> Bool
todosSonDigitos [] = True
todosSonDigitos (c : cs) = esDigito c && todosSonDigitos cs

main :: IO ()
main = do
  a <- pedirYear
  c <- pedirColumnas
  printCalendario c a