module Provas.P1 where

-- 1) Considerando a declaração da função foo abaixo, mostre todos os passos (aplicação da função) na execução de foo 24 9

foo :: (Ord t, Num t) => t -> t -> t
foo a b
  | a == b = b
  | a > b = foo (a - b) b
  | otherwise = foo a (b - a)

-- foo 24 9 = foo 15 9
-- foo 15 9 = foo 6 9
-- foo 6 9 = foo 6 3
-- foo 6 3 = foo 3 3
-- foo 3 3 = 3

-- 2) Declare uma função que receba como parâmetro uma lista de inteiros e retorne uma lista contendo apenas os inteiros impares que ocorrem na lista

impares :: (Integral a) => [a] -> [a]
impares (x : xs)
  | x `mod` 2 == 1 = x : impares xs
  | otherwise = impares xs
impares [] = []

-- 3) Declare uma função que recaba como parametros um elemento e uma lista, a função deve retornar uma lista onde todas ocorrencias deste elemento foram removidas
remover :: (Eq a) => a -> [a] -> [a]
remover n (x : xs)
  | n == x = remover n xs
  | otherwise = x : remover n xs
remover _ [] = []

-- 4) Declare uma função que receba como parametro uma lista de dados do tipo Bool e retorne True se todos os elementos da lista forem True, caso exista um elemento False, a função deve retornar False
todos :: [Bool] -> Bool
todos (x : xs)
  | not x = False
  | otherwise = todos xs
todos [] = True

-- 5) Declare uma função que receba como parametro uma lista de duplas e retorna uma lista com os segundos elementos das duplas

segundos :: [(a1, a2)] -> [a2]
segundos ((_, b) : xs) = b : segundos xs
segundos [] = []
