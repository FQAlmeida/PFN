module Aulas.Aula16 where

-- Explicação sobre Cálculo Lambda
-- Exercicios

incr :: (Num a) => a -> a
incr x = x + 1

aplica2 :: (t -> t) -> t -> t
aplica2 f x = f (f x)

aplica2incr :: Integer -> Integer
aplica2incr = aplica2 incr

_aplicar2incr :: Integer -> Integer
_aplicar2incr = aplica2 (+ (1 :: Integer))

maiorn :: (Ord a) => (a, a) -> a
maiorn (x, y) = max x y

curry' :: (Ord a) => ((a, a) -> t) -> a -> a -> t
curry' f x y = f (x, y)

uncurriedMaiorn :: (Ord a) => a -> a -> a
uncurriedMaiorn = curry' maiorn

-- Reduções

-- (λabc.cba)zz(λwv.w)
-- (λaλbλc.cba)zz(λwλv.w)
-- (λaλbλc.cba)zz(λwλv.w)
-- (λbλc.cbz)z(λwλv.w)
-- (λc.czz)(λwλv.w)
-- (λwλv.w)zz
-- (λv.z)z
-- z

-- (λz.z)(λz.zz)(λz.zy)
-- (λz.zz)(λz.zy)
-- (λz.zy)(λz.zy)
-- (λz.zy)y
-- yy

-- (λy.y)(λx.xx)(λz.zq)
-- (λx.xx)(λz.zq)
-- (λz.zq)(λz.zq)
-- (λz.zq)q
-- qq
