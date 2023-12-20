type Fila = [Int]

type Tablero = [Fila]

tableroAjedrez :: Tablero
tableroAjedrez = replicate 8 (replicate 8 0)

tableroAjedrez1 :: Tablero
tableroAjedrez1 = replicate 3 (replicate 8 0) ++ replicate 3 (replicate 8 1) ++ replicate 2 (replicate 8 0)

ocupado :: (Int, Int) -> Tablero -> Bool
ocupado (x, y) tablero = head (drop y (head (drop x tablero))) /= 0

ocupar :: (Int, Int) -> Tablero -> Tablero
ocupar (x, y) tablero = take (x - 1) tablero ++ (head (drop x tablero)) ++ (tail (drop x tablero))

-- reina :: (Int, Int) -> Tablero -> Tablero
