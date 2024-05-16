module Aulas.Aula18 where

reduction :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
reduction f acc (x:xs) = reduction f (f acc x) xs
reduction _ acc [] = acc

-- 1) Faça uma função de redução que calcule a soma dos elementos de uma lista
somaReduction :: Integer -> [Integer] -> Integer
somaReduction = reduction (+)

-- 2) Faça uma função de redução que retorne o maior elemento de uma lista
maiorReduction :: Integer -> [Integer] -> Integer
maiorReduction = reduction max

-- 3) [0, 5, 4, 6] -> 1
mapButFirstPlusOne :: Integer
mapButFirstPlusOne = foldr (\y _ -> y + 1) 0 [0 :: Integer, 5, 4, 6]

-- 4) [0, 5, 4, 6] -> 7
mapButLastPlusOne :: Integer
mapButLastPlusOne = foldl (\_ y -> y + 1) 0 [0 :: Integer, 5, 4, 6]

