module Aulas.Aula13 where

-- Aula duvidas da lista 2

-- 1 Defina uma função que ordene uma dada lista qualquer em
-- ordem crescente.

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

-- 2 Defina uma função que recebe uma lista com valores de notas de
-- alunos (0 a 10) e retorne uma lista de tuplas com o par
-- ("Aprovado", nota) caso a nota seja maior que 7 ou ("Reprovado",
-- nota) caso a nota seja menor que 7.
-- ex: [5,10,7] => [("Reprovado",5),("Aprovado",10),("Aprovado",7)]

avaliarAlunos :: (Ord b, Num b) => [b] -> [(String, b)]
avaliarAlunos (nota : restante)
  | nota >= 7 = ("Aprovado", nota) : avaliarAlunos restante
  | otherwise = ("Reprovado", nota) : avaliarAlunos restante
avaliarAlunos [] = []

-- 1) Quais destas entradas são válidas ou não em haskell? Se estiverem erradas, corrija:
--     a. [1,2,3,[]]
--     b. [1,[2,3],4]
--     c. [[1,2,3],[]]
--     d. []:[[1,2,3],[4,5,6]]
--     e. []:[]
--     f. [4]:[3]
--     g. 4:[3]
--     h. [4]:[]

--     a. [1,2,3,[]] -> inválida -> [1,2,3] | [[1], [2], [3], []]
--     b. [1,[2,3],4] -> inválida -> [1, 2, 3, 4]
--     c. [[1,2,3],[]] -> válida
--     d. []:[[1,2,3],[4,5,6]] -> valida
--     e. []:[] -> válida
--     f. [4]:[3] -> inválida -> 4 : [3] | [4] : [[3]]
--     g. 4:[3] -> valida
--     h. [4]:[] -> válida

-- 2) Defina uma função recursive de potência na qual power x y eleva à y-ésima potência.
-- power :: Int -> Int -> Int

powerRawr :: (Ord t1, Num t1, Num t2) => t2 -> t1 -> t2
powerRawr x y
  | y <= 0 = 1
  | y == 1 = x
  | otherwise = x * powerRawr x (y - 1)

-- 3) Defina zip :: [a] -> [b] -> [(a, b)], onde zip pega duas listas e concatena, de forma que o primeiro par da lista resultante seja composta pelos dois primeiros elementos de cada lista de entrada, e assim por diante. Ex: zip [1,2,3] "abc" = [(1, 'a'), (2, 'b'), (3, 'c')]. Se uma das listas for maior que a outra, pode parar assim que a primeira acabar. Ex: zip [1,2] "abc" = [(1, 'a'), (2, 'b')].

zipper :: [a] -> [b] -> [(a, b)]
zipper (x : xs) (y : ys) = (x, y) : zipper xs ys
zipper _ _ = []

-- 4) Sobre lambda: transforme as funções abaixo para a notação lambda:
--     a.  (4+)
--     b.  (1 elem)
--     c.  (notElem “abc”)

--     a.  \x -> 4 + x
--     b.  \lista -> 1 `elem` lista
--     c.  \c -> c `notElem` “abc”

-- Exercício 01: Execute as seguintes operações utilizando o menor número de parênteses:
-- 2⋅3+5
-- 2+2⋅3+1 -> (2+2)(3+1)?
-- 3^4+5⋅25+1 -> (3^4+5)⋅(2^5+1)?

-- Exercício 02: Faça uma função mult3 x que retorne True caso a entrada seja múltiplo de 3 e False caso contrário.

mult3 :: (Integral a) => a -> Bool
mult3 x = x `mod` 3 == 0

-- Exercício 03: Faça uma função mult5 x que retorne True caso a entrada seja múltiplo de 5 e False caso contrário.

mult5 :: (Integral a) => a -> Bool
mult5 x = x `mod` 5 == 0

-- Exercício 04: Faça uma função mult35 x que retorne True caso a entrada seja múltiplo de 3 e 5 e False caso contrário.

mult3e5 :: (Integral a) => a -> Bool
mult3e5 x = mult3 x && mult5 x

-- Exercício 05: Faça um programa que retorne True caso a entrada seja menor que -1 ou (maior que 1 E múltiplo de 2), e False caso contrário.

tester1 :: (Integral a) => a -> Bool
tester1 x = x < -1 || (x > 1 && even x)

-- Exercício 06: Faça uma função que recebe um tipo Integer e retorna ele dividido por 2:
div2d :: Integer -> Double
div2d x = fromInteger x / 2

-- Exercício 07: Faa uma função que receba um ângulo a e retorne uma tupla contendo o seno da metade desse ângulo utilizando a identidade:
-- sin(x/2)=±1−cos(x)2−−−−−−√

sinHalfAngle :: (Floating b) => b -> (b, b)
sinHalfAngle a = (x, -x) where x = sqrt ((1 - cos a) / 2)

-- Exercício 08: Crie uma lista de anos bissextos desde o ano 1 até o atual.

bissextos :: [Integer]
bissextos = [x | x <- [1 .. 2024], x `mod` 4 == 0]

-- Exercício 09: Encontre os 10 primeiros anos bissextos.
primeiros10bissextos :: [Integer]
primeiros10bissextos = take 10 bissextos

-- Exercício 09: Encontre os 10 últimos anos bissextos (dica: use a função length para determinar o tamanho da lista).
ultimos10bissextos :: [Integer]
ultimos10bissextos = drop (length bissextos - 10) bissextos

-- Exercício 10: Crie uma tupla em que o primeiro elemento tem metade dos anos bissextos e o segundo elemento a outra metade.

bissextosHalfed :: ([Integer], [Integer])
bissextosHalfed = splitAt (length bissextos `div` 2) bissextos

_bissextosHalfed :: ([Integer], [Integer])
_bissextosHalfed = (take (length bissextos `div` 2) bissextos, drop (length bissextos `div` 2) bissextos)

-- Exercício 11: Crie um concatenador de strings que concatena duas strings separadas por espaço.

concatStringDumb :: [Char] -> [Char] -> [Char]
concatStringDumb (c : restante) (c2 : restante2) = c : ' ' : c2 : concatStringDumb restante restante2
concatStringDumb [] (c2 : restante2) = c2 : concatStringDumb [] restante2
concatStringDumb (c : restante) [] = c : concatStringDumb restante []
concatStringDumb [] [] = []

-- Exercício 12: Dada a string “0123456789”, crie uma lista com os dígitos em formato Integer.
strToNList :: [Char] -> [Integer]
strToNList = map (\ n -> read [n])
