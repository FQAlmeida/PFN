ehTriangulo :: (Ord a, Num a) => a -> a -> a -> Bool
ehTriangulo l1 l2 l3 = (l1 + l2 > l3) && (l1 + l3 > l2) && (l3 + l2 > l1)

tipoTriangulo :: (Ord a, Num a) => a -> a -> a -> [Char]
tipoTriangulo l1 l2 l3
  | l1 == l2 && l2 == l3 = "equilatero"
  | l1 == l2 || l2 == l3 = "isosceles"
  | otherwise = "escaleno"

triangulo :: (Ord a, Num a) => a -> a -> a -> [Char]
triangulo l1 l2 l3 = if ehTriangulo l1 l2 l3 then tipoTriangulo l1 l2 l3 else "nao eh um triangulo"

isEven n = n `mod` 2 == 0
somaPares n = sum filter isEven [0..n]
