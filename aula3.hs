fun_intervalo x = if x < 5 && x > 1 then "X está dentro do intervalo" else "X não está dentro do intervalo"

_fun_intervalo x
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

_raizes :: (  Floating a, Eq a, Ord a) => a -> a -> a -> [a]
_raizes a b delta
    | delta > 0 = [(-b + sqrt delta)/2*a, (-b - sqrt delta)/2*a]
    | delta == 0 = [-b/2*a]
    | otherwise = [ ]

raizes :: ( Floating a, Eq a, Ord a) => a -> a -> a -> [a]
raizes a b c = _raizes a b ((b**2.0)-4.0*a*c)
