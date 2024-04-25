module Aulas.Aula14 where

-- Exercício 01: Crie uma função ehTriangulo que determina se três lados x, y, z podem formar um triângulo.

ehTriangulo :: (Ord a, Num a) => a -> a -> a -> Bool
ehTriangulo a b c = a + b > c && a + c > b && b + c > a

-- Exercício 02: Crie uma função tipoTriangulo que determina o tipo do triângulo formado pelos três lados x, y, z.

tipoTriangulo :: (Eq a) => a -> a -> a -> String
tipoTriangulo a b c
  | a == b && b == c = "equilatero"
  | a == b || b == c = "isosceles"
  | otherwise = "escaleno"

-- Exercício 03: Implemente uma função que faz a multiplicação etíope entre dois números.

multEtiope :: (Integral t1, Num t2) => t1 -> t2 -> t2
multEtiope 1 n = n
multEtiope m n
  | even m = multEtiope (m `div` 2) (n * 2)
  | otherwise = n + multEtiope (m `div` 2) (n * 2)

-- Exercício 04: Faça uma função que determine se um número é primo.

ehPrimo :: (Floating a, Integral a) => a -> Bool
ehPrimo x = (x == 2) || not (null ([n | n <- [3, 5 .. sqrt x], x `mod` n == 0]))

-- Exercício 05: Faça uma função que calcule a soma dos dígitos de um número.

somaDigitos :: Integral t => t -> t
somaDigitos m
        | m < 10 = m
        | otherwise = m `mod` 10 + somaDigitos (m `div` 10)

-- Exercício 06: Faça uma função que calcule a persistência aditiva de um número.

persistenciaAditiva :: (Num a, Integral t) => t -> a
persistenciaAditiva m
    | m < 10 = 0
    | otherwise = 1 + persistenciaAditiva (somaDigitos m)

-- Exercício 07: Faça uma função que calcule o coeficiente binomial de (m,n).
fatorial :: (Num a, Enum a) => a -> a
fatorial m = product [1..m]

coefBinomial :: Integral a => a -> a -> a
coefBinomial m n = fatorial m `div` (fatorial n * fatorial (m - n))

-- Exercício 08: Faça uma função que calcule o elemento (i,j) do triângulo de pascal.

trianguloPascal :: Integer -> Integer -> Integer
trianguloPascal = coefBinomial
