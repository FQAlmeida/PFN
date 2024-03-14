funIntervalo :: (Ord a, Num a) => a -> String
funIntervalo x = if x < 5 && x > 1 then "X está dentro do intervalo" else "X não está dentro do intervalo"

_funIntervalo :: (Ord a, Num a) => a -> String
_funIntervalo x
  | x < 5 && x > 1 = "X está dentro do intervalo"
  | x == 0 = "X é zero"
  | otherwise = "X não está dentro do intervalo"

{--
    Criar função que retorne as raizes de ax^2 + bx + c
    delta = b^2 - 4ac
    delta < 0 não há raizes
    delta = 0 há 1 raiz
    delta > 0 há 2 raizes
--}

_raizes :: (Floating a, Eq a, Ord a) => a -> a -> a -> [a]
_raizes a b delta
  | delta > 0 = [(-b + sqrt delta) / (2 * a), (-b - sqrt delta) / (2 * a)]
  | delta == 0 = [-(b / (2 * a))]
  | otherwise = []

raizes :: (Floating a, Eq a, Ord a) => a -> a -> a -> [a]
raizes a b c =
  let delta = (b ** 2.0) - 4.0 * a * c
   in _raizes a b delta

raizesP :: (Floating a, Eq a, Ord a, Show a) => [a]  -> [Char]
raizesP raizes = case  raizes of
  | [r1, r2] >= 2 = "Raizes: " ++ show(head rs) ++ " " ++ show(rs!!1)
  | [r1] == 1 = "Raiz: " ++ show(head rs)
  | otherwise = "Não há raizes"

raizesPrint :: (Floating a, Ord a, Show a) => a -> a -> a -> IO ()
raizesPrint a b c = putStrLn (raizesP a b c)
