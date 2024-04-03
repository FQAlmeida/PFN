module Aulas.Aula6 where

somaEntre :: (Num a, Enum a) => a -> a -> a
somaEntre n1 n2 = sum [n1 .. n2]

_fat :: (Eq p, Num p) => p -> p
_fat 0 = 1
_fat n = n * _fat (n - 1)

fat :: (Num a, Enum a) => a -> a
fat n = product [1 .. (n + 1)]

taylorSerie :: (Floating a1, Integral a2, Enum a1) => a1 -> a2 -> a1
taylorSerie n x = sum (map (\m -> (fromIntegral x ** m) / fat m) [0 .. (n - 1)])

_taylorSerie :: (Eq p, Floating p, Enum p) => p -> p -> p
_taylorSerie 0 _ = 1
_taylorSerie n x = ((x ** n) / fat n) + _taylorSerie (n - 1) x
