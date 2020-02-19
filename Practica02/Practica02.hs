--LOGICA COMPUTACIONAL 2020-2
--Práctica 2
--Fernandez Aguilar Alex Gerardo

module Practica02 where 

import Data.List

--Definiciones

--1.1 Logica Proposicional
--Tipo de dato indice
type Indice = Int
-- Tipo de dato f ́ormula
data LP = T | F | Var Indice| Neg LP| And LP LP | Or LP LP| Imp LP LP deriving (Eq, Show)

type Evaluacion = Indice -> Bool

--1.2 Evalucaion
{--
satLP :: Evaluacion -> LP -> Bool
satLP e phi = case phi of
 T -> True
 F -> False
 Var alpha -> (e alpha)
 Neg alpha -> not(satLP e alpha)
 And alpha beta -> (satLP e alpha) && (satLP e beta)
 Or alpha beta -> (satLP e alpha) || (satLP e beta)
 Imp alpha beta -> not(satLP e alpha) || (satPL e beta)
--}
--1.3 Modelos
type Modelo = [Indice]
{--
satMod m φ= True sii m|= φ

satMod :: Modelo -> LP -> Bool
satMod m phi = case phi of
 T -> True
 F->False
 Var n -> elem n m
 Oneg alpha -> not(satMod m alpha)
 Oand alpha beta -> (satMod m alpha) && (satMod m beta)
 Oor alpha beta -> (satMod m alpha) || (satMod m beta)
 Oimp alpha beta -> not(satMod m alpha) || (satMod m beta)
--}

--2 Ejercicios

--1 
--Funcion que recive una formula y devuelve la lista de variables que hay en la formula
varForm :: LP -> [Indice]
varForm (Var ind) = [ind]
varForm (Neg a)   = nub( varForm a )
varForm (And a b) = nub( (varForm a ) ++ (varForm b) )
varForm (Or a b)  = nub( (varForm a ) ++ (varForm b) )
varForm (Imp a b) = nub( (varForm a ) ++ (varForm b) )
--como true o false es todo lo que queda lo simplifico
varForm _ = []

--2
--Funcion que obtendra el conjunto potencia de un conjunto
--al serconjunto se asegura que no habra elementos repetidos
{-

conjuntoPot:: [t] -> [[t]]
conjuntoPot [] = [[]]
conjuntoPot [a] =  [[a],[]]
conjuntoPot (x:xs) = [[x]]++(aux x xs) ++ (conjuntoPot xs) 
-}

pot :: Int -> [a] -> [[a]]
pot _ []  = [[]]
pot 0 _   = [[]]
pot n xs | n >= length xs = [xs]
pot n (x:xs) = concat [map (x:) $ pot (n-1) xs, pot n xs]


nonEmptySubsequences         :: [a] -> [[a]]
nonEmptySubsequences []      =  []
nonEmptySubsequences (x:xs)  =  [x] : foldr f [] (nonEmptySubsequences xs)
  where f ys r = ys : (x : ys) : r

--3
--funcion que devuelve si la formula es valida
esVal :: LP -> Bool

--4
--funcion que devuelve si la formula es una tautologia
esSat :: LP -> Bool

--4
--funcion quita las implicaciones
quitaImp :: LP -> LP
quitaImp (Neg a)   = quitaImp a
quitaImp (And a b) = And (quitaImp a ) (quitaImp b)
quitaImp (Or a b)  = Or  (quitaImp a ) (quitaImp b)
quitaImp (Imp a b) = And (Neg(quitaImp a ))  (quitaImp b)
-- dejamos igual lo demas que hay que son variables y True y False
quitaImp a = a