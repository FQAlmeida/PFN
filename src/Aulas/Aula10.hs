module Aulas.Aula10 where

-- Aula para fazer a lista 2

-- https://www.facom.ufu.br/~madriana/PF/Exercicios1.pdf

-- 1. Forneça uma temperatura em graus Fahrenheit a partir de uma temperatura em
-- graus Celsius.
-- O grau Fahrenheit (símbolo: °F) é uma escala de temperatura proposta por Daniel
-- Gabriel Fahrenheit em 1724. Nesta escala o ponto de fusão da água é de 32 °F e o
-- ponto de ebulição de 212 °F. Uma diferença de 1,8 grau Fahrenheit equivale à de 1 °C.

celsiusToFahrenheit :: (Fractional a) => a -> a
celsiusToFahrenheit temp = temp * 1.8 + 32

fahrenheitToCelsius :: (Fractional a) => a -> a
fahrenheitToCelsius temp = (temp - 32) / 1.8

-- 2. Uma empresa decidiu dar a seus funcionários um abono de salario, baseando-se nos
-- pontos obtidos durante o mês, de acordo com a tabela:
-- Pontos Obtidos Prêmio em R$
-- 1 a 10 100,00
-- 11 a 20 200,00
-- 21 a 30 300,00
-- 31 a 40 400,00
-- A partir de 41 500

abono :: (Ord a1, Num a1, Num a2) => a1 -> a2
abono pontos
  | pontos <= 0 = 0
  | pontos <= 10 = 100
  | pontos <= 20 = 200
  | pontos <= 30 = 300
  | pontos <= 40 = 400
  | otherwise = 500

-- 3. Considere que o preço de uma passagem de avião em um trecho pode variar
-- dependendo da idade do passageiro. Pessoas com 60 anos ou mais pagam apenas
-- 60% do preço total. Crianças até 10 anos pagam 50% e bebês (abaixo de 2 anos)
-- pagam apenas 10%. Faça uma função que tenha como entrada o valor total da
-- passagem e a idade do passageiro e produz o valor a ser pago.

precoPassagem :: (Ord a1, Num a1, Fractional a2) => a2 -> a1 -> a2
precoPassagem valor_total idade
  | idade < 2 = valor_total * 0.1
  | idade <= 10 = valor_total * 0.5
  | idade >= 60 = valor_total * 0.6
  | otherwise = valor_total

-- 4. Faça uma função que recebe um numero e retorna verdadeiro se o numero for par.

isEven :: (Integral a) => a -> Bool
isEven n = mod n 2 == 0

-- 5. Faça uma função que recebe dois valores e retorna o menor.

min2 :: (Ord a) => a -> a -> a
min2 a b = if a < b then a else b

-- 6. Faça uma função que recebe três valores e retorna o menor.

min3 :: (Ord a) => a -> a -> a -> a
min3 a b c = min2 a (min2 b c)

-- 7. Escreva uma função recursiva para calcular o fatorial de um numero natural.

fat :: (Eq t, Num t) => t -> t
fat n
  | n == 0 = 1
  | otherwise = n * fat (n - 1)

-- 8. Especifique as seguintes funções para a manipulação de listas:
-- a) nro-elementos: recebe uma lista qualquer e retorna o número de elementos na
-- lista.

nroElementos :: (Num a1) => [a2] -> a1
nroElementos (_ : xs) = 1 + nroElementos xs
nroElementos [] = 0

-- b) maior: recebe uma lista de números e retorna o maior .

maior :: (Ord t) => [t] -> t
maior (x1 : x2 : xs)
  | x1 > x2 = maior (x1 : xs)
  | otherwise = maior (x2 : xs)
maior (x1 : _) = x1
maior [] = error "Lista vazia"

-- c) conta-ocorrencias: recebe um elemento e uma lista qualquer e retorna o número
-- de ocorrências do elemento na lista.

contaOcorrencias :: (Eq t, Num a) => t -> [t] -> a
contaOcorrencias n (x : xs)
  | n == x = 1 + contaOcorrencias n xs
  | otherwise = contaOcorrencias n xs
contaOcorrencias _ [] = 0

-- d) unica-ocorrencia: recebe um elemento e uma lista e verifica se existe uma
-- única ocorrência do elemento na lista .
-- ex.:
-- unica-ocorrencia 2 [1,2,3,2] = False
-- unica-ocorrencia 2 [3,1] = False
-- unica-ocorrencia 2 [2] = True

unicaOcorrencia :: (Eq t) => t -> [t] -> Bool
unicaOcorrencia n lista = contaOcorrencias n lista == (1 :: Integer)

-- e) maiores-que: recebe um número e uma lista de números e retorna uma lista com
-- os números que são maiores do que o valor informado.
-- ex.:
-- maiores-que 10 [4 6 30 3 15 3 10 7] ==> [30 15]

maioresQue :: (Ord t) => t -> [t] -> [t]
maioresQue n (x : xs)
  | x > n = x : maioresQue n xs
  | otherwise = maioresQue n xs
maioresQue _ [] = []

-- f) concatena: recebe duas listas quaisquer e retorna uma terceira lista com os
-- elementos da primeira no início e os elementos da segunda no fim.
-- ex.:
-- concatena [] [] ==> []
-- concatena [1 2] [3 4] ==> [1 2 3 4]

concatena :: [a] -> [a] -> [a]
concatena (x : xs) lista2 = x : concatena xs lista2
concatena [] lista2 = lista2

_concatena :: [a] -> [a] -> [a]
_concatena lista1 lista2 = lista1 ++ lista2

__concatena :: [a] -> [a] -> [a]
__concatena xs lista2 = foldr (:) lista2 xs

-- g) duplica: recebe uma lista e retorna uma nova lista contendo a duplicação dos
-- elementos da lista original.
-- ex: duplica [1, 2, 3] ==> [1,1,2,2,3,3]

duplica :: [a] -> [a]
duplica (x : xs) = x : x : duplica xs
duplica [] = []
