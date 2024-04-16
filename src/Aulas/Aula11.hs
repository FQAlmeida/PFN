module Aulas.Aula11 where

-- Aula sobre tuplas

somaSubtracao :: (Num b) => b -> b -> (b, b)
somaSubtracao a b = (a + b, a - b)

listaTuplas :: [(Integer, Integer, Integer)]
listaTuplas = [(x, y, z) | x <- [0 .. 100], y <- [0 .. 100], z <- [0 .. 100], x ^ (2 :: Integer) + y ^ (2 :: Integer) == z ^ (2 :: Integer)]

listaTuplasEsfera :: [(Integer, Integer, Integer)]
listaTuplasEsfera = [(x, y, z) | x <- [0 .. 100], y <- [0 .. 100], z <- [0 .. 100], x ^ (2 :: Integer) + y ^ (2 :: Integer) + z ^ (2 :: Integer) == 9]

-- 1) Defina uma função que ordene uma dada lista qualquer em ordem crescente

_removeElemento :: (Eq t) => t -> [t] -> [t]
_removeElemento a (x : xs)
  | a == x = xs
  | otherwise = x : _removeElemento a xs
_removeElemento _ [] = []

_menor :: (Ord a) => [a] -> a
_menor (x : xs)
  | x < m = x
  | otherwise = m
  where
    m = _menor xs
_menor [] = error "Lista vazia"

ordena :: (Ord t) => [t] -> [t]
ordena lista = minElemento : ordena (_removeElemento minElemento lista)
  where
    minElemento = _menor lista

-- Defina uma função que recaba uma lista com valores de notas de alunos (0 a 10) e retorna uma lista de tuplas com o par
-- ("Aprovado", nota) caso a nota seja maior que 7, e ("Reprovado", nota) caso contrário.

listaAvaliacaoAlunos :: (Ord b, Num b) => [b] -> [(String, b)]
listaAvaliacaoAlunos (nota : restanteNotas)
  | nota >= 7 = ("Aprovado", nota) : listaAvaliacaoAlunos restanteNotas
  | otherwise = ("Reprovado", nota) : listaAvaliacaoAlunos restanteNotas
listaAvaliacaoAlunos [] = []
