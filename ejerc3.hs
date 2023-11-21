muestraFecha :: (Int, Int, Int) -> IO ()
muestraFecha (d, m, a) = putStrLn (show m ++ " de " ++ nombreMes m ++ " de " ++ show a)

nombreMes :: Int -> String
nombreMes m
  | m == 1 = "Enero"
  | m == 2 = "Febrero"
  | m == 3 = "Marzo"
  | m == 4 = "Abril"
  | m == 5 = "Mayo"
  | m == 6 = "Junio"
  | m == 7 = "Julio"
  | m == 8 = "Agosto"
  | m == 9 = "Septiembre"
  | m == 10 = "Octubre"
  | m == 11 = "Noviembre"
  | m == 12 = "Diciembre"
  | otherwise = "Mes no válido"
