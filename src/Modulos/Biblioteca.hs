module Modulos.Biblioteca where

funcaoRecursiveMult :: (Eq t1, Num t1, Num t2) => t2 -> t1 -> t2
funcaoRecursiveMult _ 0 = 0
funcaoRecursiveMult x y = x + funcaoRecursiveMult x (y - 1)

funcaoFatorial :: (Num t, Eq t) => t -> t
funcaoFatorial 0 = 1
funcaoFatorial x = x * funcaoFatorial (x - 1)
