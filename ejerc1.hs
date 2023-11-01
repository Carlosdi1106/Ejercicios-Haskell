-- 1. Dí si son bien formadas cada una de las siguientes expresiones. En caso afirmativo da su valor y en caso negativo dí de qué clase es el error (sintáctico, de tipos, …):
(3== - - 3 ) && True  -- La forma correcta es -> (3==(-(-3))) && True
False && (‘a’ == True) -- La forma correcta es -> False && ('a' == 'A')
x == True || x == False
False == (1<3) -- Esta bien

--2. ¿Para qué argumentos las siguientes funciones devuelven True?
(==9).(2+).(7*) -- Da False, introduciendo  ((==9).(2+).(7*)) 19, pero si introduces 1 da correcto siguiendo la formula matematica.
-- (7*x)+2 == 9 -> 7x==7 -> x=7/7=1
(3>).(`mod` 2) -- Da True, introduciendo ((3>).(`mod` 2)) 19

--3. Define funciones para calcular el área y el perímetro de un círculo con un radio r dado (la constante pi de Haskell indica el número π).

perimetro r = 2*pi*r

area r = pi*(r^2)

--4. Define la función agregar que dados dos números enteros x e y, compruebe si y es un dígito y en caso afirmativo lo “pegue a la derecha” de x. Ejemplo: agregar 146 3 = 1463

isDigit c = c >= 0 && c<= 9

agregar :: Int -> Int -> Int

agregar x y = if isDigit y then x*10+y else error "segundo argumento no es digito"

agregar1 x y =
if 0 <= y && y <= 9 then x*10+y
                    else error "segundo argumento no es digito"

agregar2 x y =
    | 0<=y && y<=9 = x*10+y
    | otherwise = error "segundo argumento no es digito"
agregar3 x y =
    | esDigito

--5. Define una función sumcuad que tome tres números enteros y devuelva la suma de los cuadrados de los dos mayores.

--6. Definir una función divMod::(Int,Int)->(Int,Int) que, dados dividendo y divisor, devuelva el par formado por la división y el módulo de la división entera.

--7. Define una función sigLetra::Char->Char que dada una letra del alfabeto devuelva la siguiente letra (asumir que ‘A’ sigue a ‘Z’).

sigLetra :: Char -> Char
sigLetra 'Z' = 'A'  
sigLetra c = succ c  


--8. Define una función digitoVal::Char->Int que convierta un carácter dígito a su correspondiente valor numérico.

digitoVal x = if x >= 0 && x<= 9 then x else error "No es un digito"

--9. Define la función prod::Int->Int->Int tal que prod n m devuelva el producto de los números comprendidos entre n y m.

prod x y = if x==y then 1*

--10. Una fecha se puede representar por un triple de enteros (d, m, a) donde d es el día, m es el mes y a es el año. Define una función edad que dadas dos fechas, la primera la fecha de nacimiento de una persona P y la segunda la fecha actual, devuelva la edad de P mediante un número entero de años.

edad :: (Int,Int,Int) -> (Int,Int,Int) -> Int
edad (dp,mp,ap) (d,m,a)
    | mp<m || (mp==m && dp <=d)     = a-ap
    | otherwise                     = a-ap-1

--11. Define un operador binario (|-|)::Bool->Bool->Bool que calcule el ó-exclusivo de dos valores booleanos.

infixl 2 |-|
(|-|) :: Bool -> Bool -> Bool
x |-| y = (x || y) && (not x || not y)

--12. Define una función tresIgual que decida si sus tres argumentos son iguales. ¿De qué tipo es la función? Evalúa las expresiones: tresIgual 4 5 4 y tresIgual ‘a’ ‘a’ ‘a’

tresIgual x y z = if x==y && y==z then True else False

--13. Define hms::Int->(Int,Int,Int) para calcular, a partir de un número total de segundos, la hora en formato (horas, minutos, segundos). Ejemplo: hms 11720 = (3, 15, 20)

hms :: Int ->(Int,Int,Int)
hms x = (hora,minuto,segundo)
    where
        segundo = x `mod` 60
        minutosTotales = div x 60
        minuto = minutosTotales `mod` 24
        hora = div minutosTotales 24

--14. Define una función triangulo::(Int,Int,Int)->String que, dados tres lados (x,y,z) con x<=y<=z, devuelva error “no es triangulo” si los tres valores no pueden ser lados de un triángulo. Si lo pueden ser, devolverá “escaleno” (resp. “isosceles” ó “equilatero”) si forman los lados de un triángulo escaleno (resp. isósceles ó equilátero).

-- Precondicion: el dato (x,y,z) verifica  x<=y && y<=z .Ello facilita 
-- determninar la existencia de la figura geométrica triangulo

-- error:: String -> a

triangulo :: (Int,Int,Int) -> String
triangulo (x, y, z)
              | x+y <= z         = error "no es triangulo"
              | x==y && x==z    = "equilatero"
              | x==y || y==z     = "isosceles"
              | otherwise        = "escaleno"
			  
--15. Define versiones de las funciones (&&) y (||) usando patrones para el segundo argumento. Define versiones usando patrones para ambos argumentos. Comprueba mediante ejemplos las diferencias entre las distintas versiones.
