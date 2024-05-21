module Aulas.Aula19 where

import Data.Char (toLower, toUpper)

-- 1) Construa uma função que dada uma lista de strings, transforme
-- todas as letras para maisuculas/minusculas. Usando a mesma
-- função, use uma lista de inteiros e transforme os números de 0
-- até 9 para seu equivalente em string.
-- > transforma all2min "LpGUdeSc"
-- > "lpgudesc"
-- > transforma all2mai "LpGUdeSc"
-- > "LPGUDESC"
-- > transforma num2string [1,2,3]
-- > ["um","dois","três"]

transforma :: (t -> a) -> [t] -> [a]
transforma f (x : xs) = f x : transforma f xs
transforma _ [] = []

all2min :: [Char] -> [Char]
all2min = map toLower

all2min' :: [Char] -> [Char]
all2min' = transforma toLower

all2mai :: [Char] -> [Char]
all2mai = map toUpper

all2mai' :: [Char] -> [Char]
all2mai' = transforma toUpper

num2string' :: (Eq a, Num a) => a -> String
num2string' 0 = "zero"
num2string' 1 = "um"
num2string' 2 = "dois"
num2string' 3 = "três"
num2string' 4 = "quatro"
num2string' 5 = "cinco"
num2string' 6 = "seis"
num2string' 7 = "sete"
num2string' 8 = "oito"
num2string' 9 = "nove"
num2string' _ = "não implementado"

num2string :: (Eq a, Num a) => [a] -> [String]
num2string = map num2string'

num2string'' :: (Eq a, Num a) => [a] -> [String]
num2string'' = transforma num2string'

-- 2) Utilizando a função aplicarsobre (mapear) e reduzir, para uma lista de
-- inteiros, calcule inicialmente o quadrado da lista. Em seguida divida
-- pelo valor de sua posição na lista. Finalmente some os valores de
-- toda a lista.
-- x2
-- 1
-- 1 + x2
-- 2
-- 2 + x2
-- 3
-- 3 + ... + x2
-- n
-- n

polin :: [Double] -> Double
polin termos = foldr (+) (0 :: Double) (map (\(i, x) -> x `div` i) (zip [(1 :: Double) ..] (map (** 2) termos)))

polin' :: [Double] -> Double
polin' termos = foldr ((+) . (\(i, x) -> x `div` i)) (0 :: Double) (zip [(1 :: Double) ..] (map (** 2) termos))

-- 3) Utilizando as funções implementadas anteriormente calcule o valor:
-- ex = 1 + x1
-- 1! + x2
-- 2! + x3
-- 3! + ... + xn
-- n!

fat :: (Eq a, Num a, Enum a) => a -> a
fat 0 = 1
fat n = product [1 .. n]

eulerPowX :: (Fractional b, Integral b) => b -> b
eulerPowX x = foldr (+) 0 (map (\i -> (x ^ i) / fat i) [0 ..])

eulerPowX''' :: (Fractional b, Integral b) => b -> b
eulerPowX''' x = foldr ((+) . (\i -> (x ^ i) / fat i)) 0 [0 ..]

eulerPowX'' [(i, fatorial, t) : xs] x = (i + 1, next_fat, (x ^ i) / next_fat) : (i, fatorial, t) : xs
  where
    next_fat = fatorial * i
eulerPowX'' _ _ = error "não implementado"

eulerPowX' :: Integral a => a -> a -> Double
eulerPowX' x n = foldr (+) 0 (map (\(_, _, t) -> t) (foldr (eulerPowX'' x) [(0, 1, 1)] [0 .. n]))
