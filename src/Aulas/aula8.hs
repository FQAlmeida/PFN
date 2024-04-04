-- slide de listas, 

-- 1 Construa uma lista utilizando compreensão de listas, cujo o
-- conjunto é definido por: A = {x^3|x ∈ N, x ≤ 8}

lista = [x ** 3 | x <- [1..9]] 
lista = [x ** 3 | x <- [1..55] , x <= 8] 

-- 2 Implemente uma função que recebe uma lista e retorna uma nova
-- lista contendo a duplicação dos elementos da lista original.
-- ex: duplicalist [6, 2, 3] => [6,6,2,2,3,3]

duplicalist (x:xs) = [x,x] ++ duplicalist xs
duplicalist [] = [] 

-- 3 Implemente uma função que recebe uma lista de números inteiros
-- e retorne os números ímpares.
impares (x:xs)
    | x `mod` 2 == 1 = x : impares xs
    | otherwise = impares xs
impares [] = []


_impares = filter odd

