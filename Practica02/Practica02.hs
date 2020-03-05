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
-- 1.2 Evalucaion
-- Funcion que evalua con evalucion
satLP :: Evaluacion -> LP -> Bool
satLP e phi = case phi of 
  (T) -> True
  (F) -> False
  (Var alpha) -> (e alpha)
  (Neg alpha) -> not(satLP e alpha)
  (And alpha beta) -> (satLP e alpha) && (satLP e beta)
  (Or alpha beta) -> (satLP e alpha) || (satLP e beta)
  (Imp alpha beta) -> not(satLP e alpha) || (satLP e beta)

type Modelo = [Indice]
-- 1.3 Modelos
-- satMod m φ = True sii m |= φ
--funcion que aplica ciertos valores de verdad a una formula para determinar que resulta
satMod :: Modelo -> LP -> Bool
satMod m phi = case phi of
 (T) -> True
 (F) -> False
 (Var n) -> elem n m
 (Neg alpha) -> not(satMod m alpha)
 (And alpha beta) -> (satMod m alpha) && (satMod m beta)
 (Or alpha beta) -> (satMod m alpha) || (satMod m beta)
 (Imp alpha beta) -> not(satMod m alpha) || (satMod m beta)

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
-- Funcion que obtendra el conjunto potencia de un conjunto
-- al serconjunto se asegura que no habra elementos repetidos
conjuntoPot:: [t] -> [[t]]
conjuntoPot [] = [[]]
conjuntoPot l = [] : (pot l)

--funcion auxiliar que realmente hace todo el trabajo contrullendo las combinaciones
pot :: [a] -> [[a]]
pot []      =  []
pot (x:xs)  =  [x] : foldr f [] (pot xs)
  where f ys r = ys : (x : ys) : r

--3
--funcion que devuelve si la formula es una tautologia formando todas los valores de verdad posibles de las variables
esVal :: LP -> Bool
esVal a 
 | ([True] == nub (tabla (a) (conjuntoPot(varForm a))) ) = True
 | otherwise = False

--Funcion que valua con todas las combinaciones de variables geeneradas
tabla :: LP -> [[Indice]]-> [Bool]
tabla _ [] = []
tabla a (x:xs)= (satMod (x) a) : (tabla a xs)

--4
--funcion que devuelve si la formula es satisfacible 
--es decir revisa todas sus variables y genera uan tabla para indicar que todas sean T y valuar la expresion
esSat :: LP -> Bool
esSat  a = satMod (varForm a) a

--4
--funcion quita las implicaciones
quitaImp :: LP -> LP
quitaImp (Neg a)   = quitaImp a
quitaImp (And a b) = And (quitaImp a ) (quitaImp b)
quitaImp (Or a b)  = Or  (quitaImp a ) (quitaImp b)
quitaImp (Imp a b) = Or (Neg(quitaImp a ))  (quitaImp b)
-- dejamos igual lo demas que hay que son variables y True y False
quitaImp a = a