forever x = x : forever x

foreverC x = s
  where
    s = x : s

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

pares = 2 : map (+ 2) pares

pares2 = s
  where
    s = 2 : map (+ 2) s

sumas = [sumaHasta i | i <- [1 ..]]
  where
    sumaHasta i = sum [1 .. i]

sumasCorrecto = 1 : zipWith (+) [2 ..] sumasCorrecto

-- factoriales = [fact i | i <- [0..]]

factoriales = 1 : zipWith (*) [1 ..] factoriales

milista = [listaHasta i | i <- [1 ..]]
  where
    listaHasta i = [i] ++ replicate i 0

milista2 = [1, 0] : map f milista2
  where
    f (x : r) = (x + 1 : 0 : r)
