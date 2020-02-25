module Practica3
where
type Indice = Int

data LP = T | F | Var Indice
    | Neg LP
    | And LP LP | Or LP LP
    | Imp LP LP deriving (Eq, Show)
    
quitaImp:: LP->LP
-- Ya la implementaron en su práctica 2

{-
Las 3 siguientes funciones son funciones auxiliares que deben implementar.
La función negación hace que las negaciones solo afecten a proposiciones atómicas. 
La función distrCNF aplica la sig. distrivutividad: si tienen (p ^ q) v r = (p v r) ^ (q v r) 
La función distrCNF aplica la sig. distrivutividad: si tienen (p v q) ^ r = (p ^ r) v (q ^ r)
Ojo! En ambas distributividades deben considerar otro caso.  
-}
negacion:: LP -> LP

distrCNF:: LP -> LP

distrDNF:: LP -> LP

fnn:: LP -> LP
-- Utilizan la función quitaImp y negacion. Primero utilizan quitaImp y al resultado le van a aplicar la función negacion


{-
La función que le pasamos a fnc ya NO TIENE implicaciones (ya que suponemos que ya está en fnn la fórmula que nos pasen). Entonces no deben considerar ese caso
-}
fnc:: LP -> LP

{-
La función que le pasamos a fnd ya NO TIENE implicaciones (ya que suponemos que ya está en fnn la fórmula que nos pasen). Entonces no deben considerar ese caso
-}
fnd:: LP -> LP

