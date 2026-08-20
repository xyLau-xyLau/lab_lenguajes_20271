module Laboratorio01 where

distanciaOrigen :: Double -> Double -> Double
distanciaOrigen x y = sqrt(x^2 + y^2)

sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares l = sum $ filter even [x^2 | x <- l]

aplicaTresVeces :: (a -> a) -> a -> a
aplicaTresVeces f x = f $ f $ f x

varianza2 :: Double -> Double -> Double
varianza2 x1 x2 = ((x1 - mu)^2 + (x2 - mu)^2) / 2
  where mu = (x1 + x2) / 2

clasificaTemperatura :: Int -> String
clasificaTemperatura x 
  | x < 1 = "frio extremo"
  | x <= 15 = "frio"
  | x <= 25 = "templado"
  | x <= 35 = "calido"
  | otherwise = "calor extremo"

intercala :: a -> [a] -> [a]
intercala _ [] = []
intercala a (x:[]) = [x]
intercala a (x:xs) = [x] ++ [a] ++ intercala a xs

data Expr
  = Lit Int
  | Suma Expr Expr
  | Producto Expr Expr
  deriving (Eq, Show)

evalua :: Expr -> Int
evalua (Lit x) = x
evalua (Suma x y) = evalua x + evalua y
evalua (Producto x y) = evalua x * evalua y