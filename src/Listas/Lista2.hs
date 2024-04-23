module Listas.Lista2 where

import Data.Char (chr, isAsciiUpper, ord, toLower)
import Data.List (sortBy)
import Data.Ord (Down (Down), comparing)

-- 1. Declare uma função que verifica se um elemento pertence a uma lista; a função deve retornar
-- True se o elemento estiver na lista, e False caso contrário.
-- Exemplo: pertence 3 [1, 4, 3, 2] => True

pertence :: (Eq t) => t -> [t] -> Bool
pertence n (x : xs)
  | n == x = True
  | otherwise = pertence n xs
pertence _ [] = False

_pertence :: (Foldable t, Eq a) => a -> t a -> Bool
_pertence n lista = n `elem` lista

-- 2. Declare uma função que retorne a interseção entre duas listas.
-- Exemplo: intersecao [1, 3, 5, 7, 9] [2, 5, 3, 6, 9] => [3, 5, 9]

interseccao :: (Eq a) => [a] -> [a] -> [a]
interseccao (x1 : xs1) x2
  | pertence x1 x2 = x1 : interseccao xs1 x2
  | otherwise = interseccao xs1 x2
interseccao [] _ = []

-- 3. Declare uma função que retorne o inverso de uma lista.
-- Exemplo: inverso [1, 2, 3, 4] => [4, 3, 2, 1]

inverso :: [a] -> [a]
inverso (x : xs) = inverso xs ++ [x]
inverso [] = []

_inverso :: [a] -> [a]
_inverso = reverse

-- 4. Declare uma função que retorne os n últimos elementos de uma lista.
-- Exemplo: nUltimos 3 [1, 2, 3, 4, 5, 6] => [4, 5, 6]

nUltimos :: Int -> [a] -> [a]
nUltimos n lista
  | length lista <= n = lista
  | otherwise = nUltimos n (tail lista)

_nUltimos :: Int -> [a] -> [a]
_nUltimos n lista = drop (length lista - n) lista

-- 5. Declare uma função que receba duas listas de números e crie uma lista com a soma do primeiro
-- elemento da primeira lista com o primeiro elemento da segunda lista, a soma do segundo
-- elemento da primeira lista com o segundo elemento da segunda lista, e assim sucessivamente
-- até que uma das listas termine.
-- Exemplo: soma2 [1, 2, 3, 4] [10, 20, 30] => [11, 22, 33].

soma2 :: (Num a) => [a] -> [a] -> [a]
soma2 (x1 : xs1) (x2 : xs2) = (x1 + x2) : soma2 xs1 xs2
soma2 _ _ = []

_soma2 :: (Num a) => [a] -> [a] -> [a]
_soma2 (x1 : xs1) [] = x1 : _soma2 xs1 []
_soma2 [] (x2 : xs2) = x2 : _soma2 [] xs2
_soma2 (x1 : xs1) (x2 : xs2) = (x1 + x2) : soma2 xs1 xs2
_soma2 [] [] = []

__soma2 :: [Integer] -> [Integer] -> [Integer]
__soma2 = zipWith (+)

-- 6. Declare uma função que receba como parâmetro um número n e retorne uma lista com todas
-- as potências de 2 até 2^n.
-- Exemplo: pot2 4 => [2, 4, 8, 16]

pot2 :: (Integral b, Num a) => b -> [a]
pot2 n = [2 ^ x | x <- [1 .. n]]

_pot2 :: (Integral a, Num b) => a -> [b]
_pot2 n = map (2 ^) [1 .. n]

-- 7. Declare uma função que receba duas listas previamente ordenadas e faça a intercalação
-- (merge) dos elementos tendo como resultado a junção das duas listas em uma lista também
-- ordenada, sem reordenar nenhuma das listas.
-- Exemplo: intercalacao [10, 15, 17, 20] [1, 2, 13, 15, 22] => [1, 2, 10, 13, 15, 15, 17, 20, 22]

intercalacao :: (Ord a) => [a] -> [a] -> [a]
intercalacao (x1 : xs1) (x2 : xs2)
  | x1 < x2 = x1 : intercalacao xs1 (x2 : xs2)
  | otherwise = x2 : intercalacao (x1 : xs1) xs2
intercalacao (x1 : xs1) [] = x1 : intercalacao xs1 []
intercalacao [] (x2 : xs2) = x2 : intercalacao [] xs2
intercalacao [] [] = []

-- 8. Declare uma função que retorne o menor elemento de uma lista.
-- Exemplo: menor [10, 3, 5, 2, 20] => 2

menor :: (Ord a) => [a] -> a
menor [x] = x
menor (x : xs)
  | x < menorXs = x
  | otherwise = menorXs
  where
    menorXs = menor xs
menor [] = error "Lista vazia"

-- 9. Declare uma função que receba uma lista e um elemento e retorne a lista sem a primeira
-- ocorrência desse elemento.
-- Exemplo: removerElem 1 [2, 4, 1, 3, 2, 1] => [2, 4, 3, 2, 1]

removerElem :: (Eq t) => t -> [t] -> [t]
removerElem e (x : xs)
  | e == x = xs
  | otherwise = x : removerElem e xs
removerElem _ [] = []

-- 10. Usando as declarações anteriores (menor e removerElem), declare uma função que ordene
-- os elementos de uma lista.
-- Exemplo: ordenar [32, 10, 23, 10, 12, 4] => [4, 10, 10, 12, 23, 32]

ordenar :: (Ord t) => [t] -> [t]
ordenar [] = []
ordenar lista = m : ordenar (removerElem m lista)
  where
    m = menor lista

-- 11. Declare uma função que receba um elemento e uma lista ordenada, e insira este elemento na
-- lista o colocando na posição correta, ou seja, a lista resultante deve estar ordenada. Se o
-- elemento já pertencer à lista, ele não deve ser incluído, e a lista não deve ser reordenada.
-- Exemplo: insereOrd 12 [6, 9, 10, 15, 20] => [6, 9, 10, 12, 15, 20]
insereOrd :: (Ord t) => t -> [t] -> [t]
insereOrd e (x : xs)
  | e == x = x : xs
  | e < x = e : x : xs
  | otherwise = x : insereOrd e xs
insereOrd e [] = [e]

_insereOrd :: (Ord a) => a -> [a] -> [a]
_insereOrd e (x : xs)
  | pertence e (x : xs) = x : xs
  | e < x = e : x : xs
  | otherwise = x : insereOrd e xs
_insereOrd e [] = [e]

-- 12. Declare uma função que receba um número n e uma lista, e retorne o n-ésimo elemento.
-- Exemplo: enesimo 3 [10, 20, 30, 40, 50] => 30

enesimo :: (Eq t, Num t) => t -> [a] -> a
enesimo _ [] = error "Lista menor que o índice"
enesimo 0 (x : _) = x
enesimo n (_ : xs) = enesimo (n - 1) xs

_enesimo :: Int -> [a] -> a
_enesimo n lista = lista !! n

-- 13. Declare uma função que receba um inteiro n e um elemento e e crie uma lista com n elementos
-- e.
-- Exemplo: repetir 4 10 => [10, 10, 10, 10]

repetir :: (Num t, Enum t) => t -> a -> [a]
repetir n e = [e | _ <- [1 .. n]]

_repetir :: (Eq t1, Num t1) => t1 -> t2 -> [t2]
_repetir n e
  | n == 0 = []
  | otherwise = e : _repetir (n - 1) e

__repetir :: (Num t1, Eq t1) => t1 -> t2 -> [t2]
__repetir 0 _ = []
__repetir n e = e : __repetir (n - 1) e

-- 14. Declare uma função que troque todos os caracteres de tabulação (‘\t’) por espaços em uma
-- String.
-- Exemplo: removeTab “1\tTeste” => “1 Teste”

removeTab :: [Char] -> [Char]
removeTab (letra : texto)
  | letra == '\t' = ' ' : removeTab texto
  | otherwise = letra : removeTab texto
removeTab [] = []

_removeTab :: [Char] -> [Char]
_removeTab [] = []
_removeTab ('\t' : texto) = ' ' : removeTab texto
_removeTab (letra : texto) = letra : removeTab texto

__removeTab :: [Char] -> [Char]
__removeTab texto = [if x == '\t' then ' ' else x | x <- texto]

-- 15. Declare uma função que receba uma String e converta todas as letras maiúsculas dessa String
-- em letras minúsculas.
-- Exemplo: minusculas “AbCdeF” => “abcdef”

minusculas :: [Char] -> [Char]
minusculas (letra : texto)
  | letra >= 'A' && letra <= 'Z' = chr (ord letra + 32) : minusculas texto
  | otherwise = letra : minusculas texto
minusculas [] = []

_minusculas :: [Char] -> [Char]
_minusculas (letra : texto)
  | isAsciiUpper letra = toLower letra : minusculas texto
  | otherwise = letra : minusculas texto
_minusculas [] = []

__minusculas :: [Char] -> [Char]
__minusculas = map toLower

-- 16. Declare uma função que receba uma lista de duplas e retorne uma lista com o inverso de cada
-- dupla.
-- Exemplo: inversoDupla [(1, 2), (6, 1), (4, 11)] => [(2, 1), (1, 6), (11, 4)]

inversoDupla :: [(b, a)] -> [(a, b)]
inversoDupla ((a, b) : xs) = (b, a) : inversoDupla xs
inversoDupla [] = []

_inversoDupla :: [(b, a)] -> [(a, b)]
_inversoDupla listaDupla = [inversoDuplaAux dupla | dupla <- listaDupla]
  where
    inversoDuplaAux (a, b) = (b, a)

__inversoDupla :: [(b, a)] -> [(a, b)]
__inversoDupla = map inversoDuplaAux
  where
    inversoDuplaAux (a, b) = (b, a)

-- 17. Declare uma função que receba uma lista de duplas, e retorne lista de booleanos indicando se
-- os elementos são iguais ou não.
-- Exemplo: simetrico [(1, 2), (4, 4), (3, 2)] => [False, True, False]

simetrico :: (Eq a) => [(a, a)] -> [Bool]
simetrico ((a, b) : xs) = (a == b) : simetrico xs
simetrico [] = []

-- 18. Declare uma função que converta um inteiro em um número inteiro em formato de String.
-- Exemplo: numString 126 => “126”

numString :: Int -> [Char]
numString num
  | num < 10 = [chr (ord '0' + num)]
  | otherwise = numString (num `div` 10) ++ [chr (ord '0' + num `mod` 10)]

_numString :: Int -> [Char]
_numString = show

-- 19. Declare uma função que converta uma String contendo uma sequência de dígitos para um
-- inteiro, ou seja, o inverso da questão anterior.
-- Exemplo: stringNum “102” => 102

stringNum :: [Char] -> Int
stringNum [] = 0
stringNum texto = (ord (head reversed) - ord '0') + 10 * stringNum (reverse (tail reversed))
  where
    reversed = reverse texto

_stringNum :: [Char] -> Int
_stringNum (letra : texto) = (ord letra - ord '0') * 10 ^ length texto + stringNum texto
_stringNum [] = 0

-- 20. Declare uma função que converta um inteiro em um número binário, o representado como
-- uma String.
-- Exemplo: decBin 13 => “1101”

decBin :: Int -> [Char]
decBin 0 = "0"
decBin 1 = "1"
decBin num = decBin (num `div` 2) ++ [lessDig]
  where
    lessDig = chr (ord '0' + (num `mod` 2))

-- 21. Declare uma função que converta um número binário (representado como uma String) em um
-- número inteiro.
-- Exemplo: binDec “1101” => 13

binDec :: [Char] -> Int
binDec [] = 0
binDec texto = (ord (head reversed) - ord '0') + 2 * binDec (reverse (tail reversed))
  where
    reversed = reverse texto

_binDec :: [Char] -> Int
_binDec (letra : texto) = (ord letra - ord '0') * 2 ^ length texto + binDec texto
_binDec [] = 0

-- 22. Desenvolva uma função em Haskell que permita calcular o troco em moedas para o café. Para
-- isso, a função deve receber o valor do café (Int) e o valor em dinheiro pago pelo cliente (Int),
-- e retornará uma lista de tuplas [(a, b)], tal que a é o valor da moeda, e b a quantidade de
-- moedas deste valor. São permitidas moedas de 5, 10, 20 e 50 centavos, e deve ser sempre
-- retornado moedas de maior valor antes.
-- Exemplo: trocoCafe 65 110 = [(20,2), (5,1)]

moedas :: [Integer]
moedas = sortBy (comparing Data.Ord.Down) [1, 5, 10, 20, 50]

trocoCafe :: Integer -> Integer -> [(Integer, Integer)]
trocoCafe preco recebido = _trocoCafe (recebido - preco) moedas

_trocoCafe :: (Integral t) => t -> [t] -> [(t, t)]
_trocoCafe _ [] = []
_trocoCafe 0 _ = []
_trocoCafe troco (moeda : moedasRestantes)
  | qtd == 0 = _trocoCafe troco moedasRestantes
  | otherwise = (moeda, qtd) : _trocoCafe (troco - qtd * moeda) moedasRestantes
  where
    qtd = troco `div` moeda