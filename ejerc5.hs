-- Ejerc 1

-- a
data Arbus a = Vac | Nod (Arbus a) a (Arbus a)

foldArbus _ e Vac = e
foldArbus f e (Nod ai r ad) = f (foldArbus f e ai) r (foldArbus f e ad)

-- b
numVerf :: (a -> Bool) -> Arbus a -> Int
numVerf p = foldArbus (\ai r ad -> if p r then 1 + ai + ad else ai + ad) 0

esPar :: Int -> Bool
esPar x = x `mod` 2 == 0

arbus1 :: (Arbus Int)
arbus1 = Nod arbus2 4 arbus3

arbus2 :: (Arbus Int)
arbus2 = Nod Vac 2 Vac

arbus3 :: (Arbus Int)
arbus3 = Nod Vac 6 Vac