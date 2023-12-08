module Colas (Cola, vacia, primero, ultimo, avanza, entra, cuantos) where

-- Definición del tipo de datos para la Cola
data Cola a = Cola [a] deriving (Show)

-- Constructora para crear una cola vacía
vacia :: Cola a
vacia = Cola []

-- Operación para agregar un elemento al final de la cola
entra :: a -> Cola a -> Cola a
entra x (Cola xs) = Cola (xs ++ [x])

-- Operación para obtener el primer elemento de la cola
primero :: Cola a -> a
primero (Cola []) = error "La cola está vacía"
primero (Cola (x : _)) = x

-- Operación para obtener el último elemento de la cola
ultimo :: Cola a -> a
ultimo (Cola []) = error "La cola está vacía"
ultimo (Cola xs) = last xs

-- Operación para avanzar la cola (eliminar el primer elemento)
avanza :: Cola a -> Cola a
avanza (Cola []) = error "La cola está vacía"
avanza (Cola (_ : xs)) = Cola xs

-- Operación para obtener el número de elementos en la cola
cuantos :: Cola a -> Int
cuantos (Cola xs) = length xs
