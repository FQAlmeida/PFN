module Aulas.Aula7 where

-- 1 Construa uma lista utilizando compreensão de listas, cujo o conjunto é definido por: A = n x3|x ∈ N, x ≤ 8
listaCompr :: [Integer]
listaCompr = [x * 3 | x <- [1 .. 8]]

_listaCompr :: [Integer]
_listaCompr = map (* 3) [1 .. 8]

-- 2 Implemente uma função que recebe uma lista e retorna uma nova lista contendo a duplicação dos elementos da lista original.
-- ex: duplicalist [6, 2, 3] => [6,6,2,2,3,3]
duplicaLista :: [a] -> [a]
duplicaLista (x : xs) = [x, x] ++ duplicaLista xs
duplicaLista [] = []

_duplicaLista :: (Foldable t) => t a -> [a]
_duplicaLista = foldr (\x -> (++) [x, x]) []

-- 3 Implemente uma função que recebe uma lista de números inteiros e retorne os números ímpares.
impares :: (Integral a) => [a] -> [a]
impares (x : xs) = if x `mod` 2 == 1 then x : impares xs else impares xs
impares [] = []

_impares :: [Integer] -> [Integer]
_impares = filter odd
