module Practica3 where

type Indice = Int

data LP = T | F | Var Indice
    | Neg LP
    | And LP LP | Or LP LP
    | Imp LP LP deriving (Eq)
    --,(Show)

instance Show LP where 
  show T = "T" -- T
  show F = "F" -- F
  show (Var x)   = show x -- P 
  show (Neg p)   = "¬ " ++ show p -- ¬ P
  show (And p q) = "[" ++ show p ++ " && " ++ show q ++ "]" -- (P ∧ Q)
  show (Or p q)  = "(" ++ show p ++ "||" ++ show q ++ ")" -- (P ∨ Q)
  show (Imp p q) = "<" ++ show p ++ " -> " ++ show q ++ ">" -- (P → Q)

    
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
La función distrCNF aplica la sig. distrivutividad: si tienen (p ∧ q) ∨ r = (p ∨ r) ∧ (q ∨ r)  ,  r ∨ (p ∧ q) = (r ∨ p) ∧ (r ∨ q) 
La función distrCNF aplica la sig. distrivutividad: si tienen (p ∨ q) ∧ r = (p ∧ r) ∨ (q ∧ r)  ,  r ∧ (p ∨ q) = (r ∧ p) ∨ (r ∧ q) 
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
distrCNF (Or a b) = case (Or a b) of
 (Or (And c d)  b) -> And (distrCNF (Or c b)) (distrCNF (Or d b))
 (Or a (And c d) ) -> And (distrCNF (Or a c)) (distrCNF (Or a d)) 
 (Or a b) -> (Or (distrCNF a) (distrCNF b) )
distrCNF a = a

distrDNF:: LP -> LP
distrDNF (Or a b) = (And (distrDNF a ) (distrDNF b) )
distrDNF (Imp a b) = (Imp (distrDNF a ) (distrDNF b) )
distrDNF (And a b) = case (And a b) of
 (And (Or c d)  b) -> Or (distrDNF (And c b)) (distrDNF (And d b))
 (And a (Or c d) ) -> Or (distrDNF (And a c)) (distrDNF (And a d))
 (And a b) -> (And (distrDNF a) (distrDNF b) )
distrDNF a = a

--Ejercicio 1
fnn:: LP -> LP
-- Utilizan la función quitaImp y negacion. Primero utilizan quitaImp y al resultado le van a aplicar la función negacion
--dado que implemente en la negacion el quitar la implicacion
fnn lp = negacion( lp )

--Ejercicio 2
--La función que le pasamos a fnc ya NO TIENE implicaciones (ya que suponemos que ya está en fnn la fórmula que nos pasen). Entonces no deben considerar ese caso
fnc:: LP -> LP
fnc lp = (distrCNF (negacion lp) )

--Ejercicio 3
--La función que le pasamos a fnd ya NO TIENE implicaciones (ya que suponemos que ya está en fnn la fórmula que nos pasen). Entonces no deben considerar ese caso
fnd:: LP -> LP
fnd lp = (distrDNF (negacion lp) )

--Ejercicio 4
--Funcion que verificara si esta en una forma normal conjuntiva
valCNF :: LP -> Bool

--Ejercicio 5
--Funcion que verificara si esta en una forma normal disyuntiva
valDNF :: LP -> Bool