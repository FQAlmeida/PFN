module Aulas.Aula5 where

fib :: (Num a, Eq a) => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

memoizedFib :: Int -> Integer
memoizedFib = (map mfib [0 ..] !!)
  where
    mfib 0 = 0
    mfib 1 = 1
    mfib n = memoizedFib (n - 2) + memoizedFib (n - 1)

soma :: (Eq p, Num p) => p -> p
soma 0 = 0
soma n = n + soma (n - 1)

somaEntre :: (Eq p, Num p, Ord p) => p -> p -> p
somaEntre a b | a >= b = 0
somaEntre a b = a + somaEntre (a + 1) b

seqRoot :: (Eq t, Floating p, Num t) => t -> p
seqRoot 1 = sqrt 6
seqRoot n = sqrt (6 + seqRoot (n - 1))
