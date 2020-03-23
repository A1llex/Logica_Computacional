--LOGICA COMPUTACIONAL 2020-2
--Práctica 2
--Fernandez Aguilar Alex Gerardo

module Practica03 where

type Indice = Int

data LP = T | F | Var Indice
    | Neg LP
    | And LP LP | Or LP LP
    | Imp LP LP deriving (Eq)

instance Show LP where 
  show T = "T" -- T
  show F = "F" -- F
  show (Var x)   = show x -- P 
  show (Neg p)   = "¬ " ++ show p -- ¬ P
  show (And p q) = "[" ++ show p ++ "&&" ++ show q ++ "]" -- (P ∧ Q)
  show (Or p q)  = "(" ++ show p ++ "||" ++ show q ++ ")" -- (P ∨ Q)
  show (Imp p q) = "(" ++ show p ++ "->" ++ show q ++ ")" -- (P → Q)

--Funcion de la tarea anterior
quitaImp :: LP -> LP
quitaImp (Neg a)   = quitaImp a
quitaImp (And a b) = And (quitaImp a ) (quitaImp b)
quitaImp (Or a b)  = Or  (quitaImp a ) (quitaImp b)
quitaImp (Imp a b) = Or (Neg(quitaImp a ))  (quitaImp b)
-- dejamos igual lo demas que hay que son variables y True y False
quitaImp a = a

{-
Las 3 siguientes funciones son funciones auxiliares que deben implementar.
La función negación hace que las negaciones solo afecten a proposiciones atómicas. 
La función distrCNF aplica la sig. distrivutividad: si tienen (p ∧ q) ∨ r = (p ∨ r) ∧ (q ∨ r)
La función distrCNF aplica la sig. distrivutividad: si tienen (p ∨ q) ∧ r = (p ∧ r) ∨ (q ∧ r)
Ojo! En ambas distributividades deben considerar otro caso.  
-}

negacion:: LP -> LP
--dentro de la funcion tambien implemente quitar las implicaciones
negacion (And a b) = (And (negacion a ) (negacion b) )
negacion (Or a b)  = (Or  (negacion a ) (negacion b) )
negacion (Imp a b) = (Or (negacion (Neg a) ) (negacion b) )
negacion (Neg a) = case a of
 (Neg alpha) -> (negacion (alpha))
 (T) -> F
 (F) -> T
 (And a b) -> (Or (negacion (Neg a) ) (negacion (Neg b) ) )
 (Or a b)  -> (And (negacion (Neg a) ) (negacion (Neg b) ) )
 (Imp a b) -> (And (negacion a ) (negacion (Neg b) ) )
 a -> (Neg ( negacion a))
negacion a = a

distrCNF:: LP -> LP
distrCNF (And a b) = (And (distrCNF a ) (distrCNF b) )
distrCNF (Imp a b) = (Imp (distrCNF a ) (distrCNF b) )
distrCNF (Or a b)  = case (Or a b) of
 (Or (And c d)  b) -> And (distrCNF (Or c b)) (distrCNF (Or d b))
 (Or a (And c d) ) -> And (distrCNF (Or a c)) (distrCNF (Or a d)) 
 _ -> (Or (distrCNF a) (distrCNF b) )
distrCNF a = a

distrDNF:: LP -> LP
distrDNF (Or a b)  = (Or (distrDNF a ) (distrDNF b) )
distrDNF (Imp a b) = (Imp (distrDNF a ) (distrDNF b) )
distrDNF (And a b) = case (And a b) of
 (And (Or c d)  b) -> Or (distrDNF (And c b)) (distrDNF (And d b))
 (And a (Or c d) ) -> Or (distrDNF (And a c)) (distrDNF (And a d))
 _ -> (And (distrDNF a) (distrDNF b) )
distrDNF a = a

--Ejercicio 1
fnn:: LP -> LP
-- Utilizan la función quitaImp y negacion. Primero utilizan quitaImp y al resultado le van a aplicar la función negacion
-- dado que implemente en la negacion el quitar la implicacion queda asi
fnn lp = negacion( lp )

--Ejercicio 2
--La función que le pasamos a fnc ya NO TIENE implicaciones (ya que suponemos que ya está en fnn la fórmula que nos pasen). Entonces no deben considerar ese caso
fnc:: LP -> LP
-- es necesario aplicarla dos veces por las veces rezagadas que pasan al usar la distivutibidad
fnc lp = distrCNF (distrCNF (negacion lp) )

--Ejercicio 3
--La función que le pasamos a fnd ya NO TIENE implicaciones (ya que suponemos que ya está en fnn la fórmula que nos pasen). Entonces no deben considerar ese caso
fnd:: LP -> LP
-- es necesario aplicarla dos veces por las veces rezagadas que pasan al usar la distivutibidad
fnd lp = distrDNF (distrDNF (negacion lp) )

--Ejercicio 4
valCNF :: LP -> Bool
-- es necesario aplicarla dos veces por las veces rezagadas que pasan al simplifocar casos
valCNF lp = auxvalCNF(simp (simp lp))

auxvalCNF :: LP -> Bool
auxvalCNF (And a b ) 
  |(a == F )  = False
  |(b == F )  = False
  |(a == (Neg b ) ) = False
  |(b == (Neg a ) ) = False
  |otherwise = ( (auxvalCNF a) && (auxvalCNF b) )
auxvalCNF (Or a b ) 
  |(a == T )  = True 
  |(b == T )  = True
  |(a == (Neg b ) ) = True
  |(b == (Neg a ) ) = True
  |otherwise = ( (auxvalCNF a) || (auxvalCNF b) )
auxvalCNF T = True
auxvalCNF F = False
auxvalCNF _ = False

--Funcion para ver si hay para simplificar 
simp :: LP -> LP
simp (Or a (Or b c) )
 |(a == T)  = T
 |(b == T)  = T
 |(c == T)  = T
 |((Neg a) ==  b ) = T
 |(a ==  (Neg b) ) = T
 |((Neg a) ==  c ) = T
 |(a ==  (Neg c) ) = T
 |((Neg b) == c )  = T
 |(b ==  (Neg c) ) = T
 |otherwise = (Or (simp a) (Or (simp b) (simp c) ) ) 
simp (Or (Or a b) c )
 |(a == T)  = T
 |(b == T)  = T
 |(c == T)  = T
 |((Neg a) ==  b ) = T
 |(a ==  (Neg b) ) = T
 |((Neg a) ==  c ) = T
 |(a ==  (Neg c) ) = T
 |((Neg b) == c )  = T
 |(b ==  (Neg c) ) = T
 |otherwise = (Or (simp a) (Or (simp b) (simp c) ) ) 
simp (Or a b )
  |(a == T)  = T
  |(b == T)  = T
  |(a ==  (Neg b) ) = T
  |(b ==  (Neg a) ) = T
  |otherwise = (Or (simp a) (simp b) )
simp (And a b ) 
  |(a == F )  = F
  |(b == F )  = F
  |(a == (Neg b) ) = F
  |(b == (Neg a) ) = F
  |otherwise = (And (simp a) (simp b) )
simp (Neg T) = F
simp (Neg F) = T
simp a = a

--Ejercicio 5
satDNF :: LP -> Bool
satDNF lp = auxvalDNF(simp (lp))

auxvalDNF :: LP -> Bool
auxvalDNF (And a b )
  |(a == F )  = False
  |(b == F )  = False
  |(a == (Neg b ) ) = False
  |(b == (Neg a ) ) = False
  |otherwise = case (And a b ) of
    (And (Var x) (Var y)) -> True
    (And (Var x) y) -> ( (auxvalDNF a) && True )
    (And x (Var y)) -> ( True && (auxvalDNF b) )
    _ -> ( (auxvalDNF a) && (auxvalDNF b) )
auxvalDNF (Or a b ) 
  |(a == T )  = True 
  |(b == T )  = True
  |(a == (Neg b ) ) = True
  |(b == (Neg a ) ) = True
  |otherwise = case (Or a b ) of
    (Or x (Var y)) -> True
    (Or (Var x) y) -> True
    _ -> ( (auxvalDNF a) || (auxvalDNF b) )
auxvalDNF T = True
auxvalDNF F = False
auxvalDNF _ = False

--Ejercicio extra
--Funcion que verificara si esta en una forma normal conjuntiva
esCNF :: LP -> Bool
esCNF (Imp _ _) = False
esCNF (And a b)  = (esCNF a) && (esCNF b)
esCNF (Neg a) = case (Neg a) of
 (Neg (Imp _ _) ) -> False
 (Neg (Or _ _) )  -> False
 (Neg (And _ _) ) -> False
 (Neg (Neg _) ) -> False
 _ -> True
esCNF (Or a b) = case (Or a b) of
 (Or (And _ _)  _) -> False
 (Or _ (And _ _) ) -> False
 (Or a b) -> (esCNF a) && (esCNF b)
esCNF a = True

--Ejercicio extra
--Funcion que verificara si esta en una forma normal disyuntiva
esDNF :: LP -> Bool
esDNF (Imp _ _) = False
esDNF (Or a b)  = (esDNF a) && (esDNF b)
esDNF (Neg a) = case (Neg a) of
 (Neg (Imp _ _) ) -> False
 (Neg (Or _ _) )  -> False
 (Neg (And _ _) ) -> False
 (Neg (Neg _) ) -> False
 _ -> True
esDNF (And a b) = case (And a b) of
 (And (Or _ _)  _ ) -> False
 (And _ (Or _ _ ) ) -> False
 (And a b) -> (esDNF a) && (esDNF b)
esDNF a = True