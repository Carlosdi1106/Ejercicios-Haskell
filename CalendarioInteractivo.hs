-----------------------------------------------------------------
-- PRACTICA CALENDARIO INTERACTIVO     2023-24
-----------------------------------------------------------------

-- La llamada principal debe ser main

module CalendarioInteractivo where

import Calendario -- importar el modulo de la Practica 1

-------------------------------------


-- Función para validar el año
validarAnio :: Int -> Bool
validarAnio anio = anio > 0 && anio < 2500

-- Función para validar el número de columnas
validarColumnas :: Int -> Bool
validarColumnas columnas = columnas == 3 || columnas == 4

-- Función principal para obtener el año del usuario
pedirAnio :: IO Int
pedirAnio = do
  putStr "Ingrese el año para el calendario: "
  anioInput <- getLine
  let anio = read anioInput :: Int
  if validarAnio anio
    then return anio
    else do
      putStrLn "Año no válido. Debe ser un año mayor que 0 y menor que 2500."
      pedirAnio

-- Función principal para obtener el número de columnas del usuario
pedirColumnas :: IO Int
pedirColumnas = do
  putStr "Ingrese el número de columnas (3 ó 4) para el calendario: "
  columnasInput <- getLine
  let columnas = read columnasInput :: Int
  if validarColumnas columnas
    then return columnas
    else do
      putStrLn "Número de columnas no válido. Debe ser 3 ó 4."
      pedirColumnas

main :: IO ()
main = do
  a <- pedirAnio
  c <- pedirColumnas
  printCalendario c a