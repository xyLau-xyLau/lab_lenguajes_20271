module Laboratorio06 where

-- 1. Potencia --------------------------------------------------------------

-- Precondicion: el exponente es mayor o igual que cero.
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia base exponente =
  base * potencia base (exponente - 1)

potenciaCola :: Int -> Int -> Int
potenciaCola base exponente = potenciaCola' base exponente 1

potenciaCola' :: Int -> Int -> Int -> Int
potenciaCola' base 0 acc = acc
potenciaCola' base exponente acc = 
  potenciaCola' base (exponente - 1) (acc * base)

potenciaCPS :: Int -> Int -> (Int -> r) -> r
potenciaCPS base 0 k = k 1
potenciaCPS base exponente k = 
  potenciaCPS base (exponente - 1) (\v -> k (base * v))

-- 2. Longitud de una lista -------------------------------------------------

longitud :: [a] -> Int
longitud [] = 0
longitud (_ : xs) = 1 + longitud xs

longitudCola :: [a] -> Int
longitudCola xs = longitudCola' xs 0

longitudCola' :: [a] -> Int -> Int
longitudCola' [] acc = acc
longitudCola' (x:xs) acc = longitudCola' xs (acc + 1)

longitudCPS :: [a] -> (Int -> r) -> r
longitudCPS [] k = k 0
longitudCPS (x:xs) k = longitudCPS xs (\v -> k (v + 1))

-- 3. Reversa de una lista --------------------------------------------------

reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x]

reversaCola :: [a] -> [a]
reversaCola xs = reversaCola' xs []

reversaCola' :: [a] -> [a] -> [a]
reversaCola' [] acc = acc
reversaCola' (x:xs) acc = reversaCola' xs (x:acc)

-- NOTA: No encontré manera de optimizar la concatenación
-- al llamar la continuación :C 
reversaCPS :: [a] -> ([a] -> r) -> r
reversaCPS [] k = k []
reversaCPS (x:xs) k = reversaCPS xs (\v -> k (v ++ [x]))

-- 4. Concatenacion de una lista de listas ---------------------------------

concatena :: [[a]] -> [a]
concatena [] = []
concatena (xs : xss) = xs ++ concatena xss

-- NOTA: Para optimizar la construcción del acumulador evito usar ++ y opto por recorrer
-- las listas individualmente y concatenar con :. Al obtener el resultado se aplica reversa.
-- De no ser válido usar reversa anexo comentada la solución alternativa
concatenaCola :: [[a]] -> [a]
concatenaCola xs = reversaCola(concatenaCola' xs [])

concatenaCola' :: [[a]] -> [a] -> [a]
concatenaCola' [] acc = acc
concatenaCola' ([]:xs) acc = concatenaCola' xs acc
concatenaCola' ((a:as):xs) acc = concatenaCola' (as:xs) (a:acc) 

{-
concatenaCola :: [[a]] -> [a]
concatenaCola xs = concatenaCola' xs []

concatenaCola' :: [[a]] -> [a] -> [a]
concatenaCola' [] acc = acc
concatenaCola' (x:xs) acc = concatenaCola' xs (acc ++ x)
-}
concatenaCPS :: [[a]] -> ([a] -> r) -> r
concatenaCPS [] k = k []
concatenaCPS (x:xs) k = concatenaCPS xs (\v -> k (x ++ v))

-- 5. Intercalado de dos listas --------------------------------------------

intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x : xs) (y : ys) =
  x : y : intercala xs ys

intercalaCola :: [a] -> [a] -> [a]
intercalaCola xs ys = intercalaCola' xs ys []

-- NOTA: Para optimizar la construcción del acumulador evito usar ++, y opto por construirlo 
-- en reversa, posteriormente al caer en caso base le aplico la reversa, 
-- de no ser válido usar reversa anexo comentada la solución alternativa
intercalaCola' :: [a] -> [a] -> [a] -> [a]
intercalaCola' [] ys acc = reversaCola(acc) ++ ys
intercalaCola' xs [] acc = reversaCola(acc) ++ xs
intercalaCola' (x:xs) (y:ys) acc = intercalaCola' xs ys (y:x:acc)

{-
intercalaCola' :: [a] -> [a] -> [a] -> [a]
intercalaCola' [] ys acc = acc ++ ys
intercalaCola' xs [] acc = acc ++ xs
intercalaCola' (x:xs) (y:ys) acc = intercalaCola' xs ys (acc ++ [x,y])
-}

intercalaCPS :: [a] -> [a] -> ([a] -> r) -> r
intercalaCPS [] ys k = k ys
intercalaCPS xs [] k = k xs
intercalaCPS (x:xs) (y:ys) k = intercalaCPS xs ys (\v -> k (x:y:v))
