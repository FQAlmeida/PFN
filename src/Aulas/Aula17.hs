module Aulas.Aula17 where

mapear :: (t -> a) -> [t] -> [a]
mapear f (x : xs) = f x : mapear f xs
mapear _ [] = []

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar f (x : xs)
  | f x = x : filtrar f xs
  | otherwise = filtrar f xs
filtrar _ [] = []

reduzir :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
reduzir f acc (x : xs) = reduzir f (f acc x) xs
reduzir _ acc [] = acc

reduzirProbAcc :: [Double] -> [Double]
reduzirProbAcc = reduzir (\x y -> x ++ [last x + y]) []
