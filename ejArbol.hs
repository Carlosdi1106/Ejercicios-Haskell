data ArbGen a = Vac | Nodo a [ArbGen a] deriving (Eq, Show)

a1 :: ArbGen Int
a1 = Nodo 10 [a2, a5, a6]

a2 :: ArbGen Int
a2 = Nodo 22 [a3, a4]

a3 :: ArbGen Int
a3 = Nodo 15 [Vac]

a4 :: ArbGen Int
a4 = Nodo 12 [Vac]

a5 :: ArbGen Int
a5 = Nodo 35 [Vac]

a6 :: ArbGen Int
a6 = Nodo 52 [a7]

a7 :: ArbGen Int
a7 = Nodo 33 [Vac]

printArbGen :: Show a => ArbGen a -> String
printArbGen Vac = ""
printArbGen (Nodo x []) = show x
printArbGen (Nodo x children) =
  show x ++ " [" ++ concatMap printArbGen children ++ "]"

contarNodos :: ArbGen a -> Int
contarNodos Vac = 0
contarNodos (Nodo _ children) = 1 + sum (map contarNodos children)

