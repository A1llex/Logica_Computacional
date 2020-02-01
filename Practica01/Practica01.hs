--LOGICA COMPUTACIONAL 2020-2
--Práctica 2
--Fernandez Aguilar Alex Gerardo

--Definiciones
data Natural = Cero | Suc Natural deriving Show
data ListaNat = Nil | Cons Natural ListaNat deriving Show
data BTree a = Void | Node ( BTree a ) a ( BTree a ) deriving Show
data ListaSnoc a = Empty | Snoc ( ListaSnoc a ) a deriving Show


--Ejercicio 1
--Funcion que regresa si un numero "Natural" es estrictamente mayor que otro
mayorQue :: Natural -> Natural -> Bool
mayorQue Cero Cero = False
mayorQue Cero b = False
mayorQue a Cero = True
mayorQue (Suc a) (Suc b)  = mayorQue a b


--Ejercicio 2
--Funcion que le restara al primer Natural el segundo Natural
restaNat :: Natural -> Natural -> Natural
restaNat Cero Cero = error "El Ejercicio Pide que sea estrictamente mayor el primer Natural"
restaNat Cero b = error "Es necesario que el primer Natural sea mayor al segundo Natural"
restaNat a Cero = a
restaNat (Suc a) (Suc b) = restaNat a b

--Ejercicio 3
--Funcion que Multiplica dos Naturales
mulNat :: Natural -> Natural -> Natural
mulNat Cero Cero = Cero
mulNat a Cero = Cero
mulNat Cero b = Cero
mulNat a (Suc Cero) = a
mulNat a (Suc b) = sumNat (a) (mulNat a b)

--Funcion auxiliar Suma de dos Naturales
sumNat :: Natural -> Natural -> Natural
sumNat a Cero = a
sumNat Cero b = b
sumNat a (Suc b) = Suc(sumNat a b)

--Ejercicio 4
--Funcion que regresa la reversa de una lista
reversa :: ListaNat -> ListaNat
reversa Nil = Nil
reversa (Cons a  Nil) = (Cons a  Nil)
reversa (Cons a  b) = concatena (reversa b)  (Cons a Nil) 

--Ejercicio 5
--Funcion que concatena dos LisaNat una seguida de la segunda
concatena :: ListaNat -> ListaNat -> ListaNat
concatena Nil b = b
concatena a Nil = a
concatena (Cons a as) b =  Cons ( a ) (concatena  as  b)

--Ejercicio 6
--Funcion que busca e indica si un Natural esta dentro de una listaNat
pertenece :: Natural -> ListaNat ->Bool
pertenece a  Nil = False
pertenece a  (Cons b bs) 
 | (igual a b) = True
 | otherwise = (pertenece a bs)

--Funcion auxiliar para saber si dos Naurales son el mismo 
igual :: Natural -> Natural -> Bool
igual Cero Cero = True
igual a Cero = False
igual Cero b = False
igual (Suc a) (Suc b) = igual a b


