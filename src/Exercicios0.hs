-- Declare uma função que retorne o valor de f(x) para a função f (x ) = x 2 + 2x + 3
funcao1 :: Floating a => a -> a
funcao1 x = x ** 2 + 2 * x + 3

-- Declare uma função que retorne o valor do módulo de um vetor no espaço R3.
modR3 :: Floating a => a -> a -> a -> a
modR3 x y z = sqrt (x ** 2 + y ** 2 + z ** 2)

-- Declare uma função que compara se um valor é maior que 100
maior100 :: (Ord a, Num a) => a -> Bool
maior100 x = x > 100

-- Declare uma função que retorna a soma de 4 números x + y + z + w
sum4 :: Num a => a -> a -> a -> a -> a
sum4 x y z w = x + y + z + w

-- Declare uma função que calcula o produto escalar entre dois vetores.⃗ v1 ·⃗ v2 = (x1 · x2 + y1 · y2 + z1 · z2)
prodEscalar :: Num a => a -> a -> a -> a -> a -> a -> a
prodEscalar x1 y1 z1 x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2

-- Declare uma função que calcula a porcentagem de um valor. Ex: 7% de 100. porcentagem 7 100 = 7
porcentagem :: Fractional a => a -> a -> a
porcentagem x y = (x / 100) * y

-- Declare uma função que retorna o número de múltiplos de 7 entre 0 e o valor informado pelo usuário.
multiplos7 :: Integral a => a -> a
multiplos7 x = sum (filter (\n -> n `mod` 7 == 0) [0 .. x])

_multiplos7 :: (Num a, Enum a) => a -> Int
_multiplos7 x = length [7, 14 .. x]

-- Defina uma função que calcula a potência de um valor x elevado a y utilizando recursão.
potencia :: (Eq t, Num t, Num p) => p -> t -> p
potencia _ 0 = 1
potencia x y = x * potencia x (y - 1)

-- Defina uma função que calcula a soma dos quadrados de um valor de entrada x. Ex: somaq 3 = 3^2 + 2^2 + 1^2 + 0^2
somaq :: (Num p, Eq p) => p -> p
somaq 0 = 0
somaq x = x ^ (2 :: Integer) + somaq (x - 1)

_somaq :: (Num a, Enum a) => a -> a
_somaq x = sum (map (^ (2 :: Integer)) [0 .. x])

-- Defina uma função que faz a soma dos valores múltiplos de 3 do valor x de entrada até zero.
soma3 :: Integral a => a -> a
soma3 0 = 0
soma3 x = if x `mod` 3 == 0 then x + soma3 (x - 1) else soma3 (x - 1)

_soma3 :: Integral a => a -> a
_soma3 x = sum (filter (\n -> n `mod` 3 == 0) [0 .. x])

__soma3 :: Integral a => a -> a
__soma3 x = sum [3, 6 .. x]

-- Defina uma função que calcula a raiz inteira de um número inteiro. Ex: √65 = 8
isqrt :: (Integral b, Integral a) => a -> b
isqrt n = floor (sqrt (fromIntegral n) :: Double)

_isqrt :: (Num t, Ord t) => t -> t
_isqrt n = __isqrt n 0

__isqrt :: (Num t, Ord t) => t -> t -> t
__isqrt 0 _ = 0
__isqrt n x = if x * x > n then x - 1 else __isqrt n (x + 1)
