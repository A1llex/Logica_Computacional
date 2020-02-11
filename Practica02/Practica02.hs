--LOGICA COMPUTACIONAL 2020-2
--Práctica 2
--Fernandez Aguilar Alex Gerardo

module Practica02 where 

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
varForm T = []
varForm F = []
varForm (Var ind) = [ind]
varForm (Neg a) = (varForm a)
varForm (And a b) = (varForm a) ++ (varForm b)
varForm (Or a b) = (varForm a) ++ (varForm b)
varForm (Imp a b) = (varForm a) ++ (varForm b)


--Funcion auxiliar que suma los ultimos 3 elementos de las listas