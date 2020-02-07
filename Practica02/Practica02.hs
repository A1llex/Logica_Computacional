--LOGICA COMPUTACIONAL 2020-2
--Práctica 2
--Fernandez Aguilar Alex Gerardo

module Practica02 where 

--Definiciones
data Natural = Cero | Suc Natural deriving Show
data ListaNat = Nil | Cons Natural ListaNat --deriving Show
data BTree a = Void | Node ( BTree a ) a ( BTree a ) deriving Show
data ListaSnoc a = Empty | Snoc ( ListaSnoc a ) a deriving Show


--Ejercicio 1
--Funcion que regresa si un numero "Natural" es estrictamente mayor que otro


--Funcion auxiliar que suma los ultimos 3 elementos de las listas
sumtreslist :: [Int] -> Int
sumtreslist [] = 0
sumtreslist [a] = a
sumtreslist [a,b] = a+b
sumtreslist [a,b,c] = a+b+c
sumtreslist (a:b) = sumtreslist( b)