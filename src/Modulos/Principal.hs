module Modulos.Principal where

import Modulos.Biblioteca

_main :: IO ()
_main = do
    putStrLn "Digite um número para calcular o fatorial: "
    x <- getLine 
    let xInt = read x :: Int
    let fatorial = funcaoFatorial xInt
    putStrLn ("O fatorial de " ++ x ++ " é " ++ show fatorial)

    putStrLn "Digite um número para multiplicar: "
    y <- getLine
    putStrLn "Digite outro número para multiplicar: "
    z <- getLine
    let yInt = read y :: Int
    let zInt = read z :: Int
    let mult = funcaoRecursiveMult yInt zInt
    putStrLn ("O resultado da multiplicação de " ++ y ++ " por " ++ z ++ " é " ++ show mult)