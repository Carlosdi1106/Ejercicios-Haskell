module Supermercado where

-- module Supermercado (irColaPago, salirColaPago) where
import Colas

-- irColaPago -> Entra en la Cola que sea mas corta
-- salirColaPago -> Saca el primer elemento de la Cola cuando ha pagado.

type Nombre = [Char]

type Apellido = [Char]

type Edad = Integer

type Persona = (Nombre, Apellido, Edad)

type Supermercado = (Cola Persona, Cola Persona, Cola Persona)

cajaCobro1 = vacia

cajaCobro2 = vacia

cajaCobro3 = vacia

persona1 = ("Juan", "Perez", 20)

persona2 = ("Maria", "Gomez", 30)

persona3 = ("Pedro", "Lopez", 40)

persona4 = ("Judith", "Martinez", 50)

super :: Supermercado
super = (cajaCobro1, cajaCobro2, cajaCobro3)

-- irColaPago ::

irColaPago :: Persona -> Supermercado -> Supermercado
irColaPago persona (caja1, caja2, caja3)
  | cuantos caja1 <= cuantos caja2 && cuantos caja1 <= cuantos caja3 = (cajaNueva caja1, caja2, caja3)
  | cuantos caja2 <= cuantos caja1 && cuantos caja2 <= cuantos caja3 = (caja1, cajaNueva caja2, caja3)
  | otherwise = (caja1, caja2, cajaNueva caja3)
  where
    cajaNueva caja = entra persona caja

salirColaPago :: Cola Persona -> Supermercado -> Supermercado
salirColaPago cola (caja1, caja2, caja3)
  | primero cola == primero caja1 = (avanza caja1, caja2, caja3)
  | primero cola == primero caja2 = (caja1, avanza caja2, caja3)
  | otherwise = (caja1, caja2, avanza caja3)
