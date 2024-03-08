ehTriangulo :: (Ord a, Num a) => a -> a -> a -> Bool
ehTriangulo l1 l2 l3 = (l1 + l2 > l3) && (l1 + l3 > l2) && (l3 + l2 > l1)

tipoTriangulo :: (Ord a, Num a) => a -> a -> a -> [Char]
tipoTriangulo l1 l2 l3
  | l1 == l2 && l2 == l3 = "equilatero"
  | l1 == l2 || l2 == l3 = "isosceles"
  | otherwise = "escaleno"

triangulo :: (Ord a, Num a) => a -> a -> a -> [Char]
triangulo l1 l2 l3 = if ehTriangulo l1 l2 l3 then tipoTriangulo l1 l2 l3 else "nao eh um triangulo"

_triangulo :: (Ord a, Num a) => a -> a -> a -> [Char]
_triangulo l1 l2 l3
  | ehTriangulo l1 l2 l3 = tipoTriangulo l1 l2 l3
  | otherwise = "nao eh um triangulo"

-- [0..3] -> [0, 1, 2, 3]
-- [0..n] -> [0, 1, 2, ..., n-1, n]
somaPares :: (Num a, Enum a, Integral a) => a -> a
somaPares n = sum (filter even [0 .. n])

somaPot2m :: (Eq a, Integral a) => a -> a -> a
somaPot2m m n
  | n == 0 = m
  | otherwise = 2 ^ n * m + somaPot2m m (n - 1)

_somaPot2m :: (Eq a, Integral a, Enum a) => a -> a -> a
_somaPot2m m n = sum (map (\n -> 2 ^ n * m) [0 .. n])

isDiv :: (Num a, Integral a) => a -> a -> Bool
isDiv num div = (num `mod` div) == 0

isqrt :: (Integral t) => t -> t
isqrt = floor . sqrt . fromIntegral

_primo :: (Num t, Eq t, Integral t) => t -> t -> Bool
_primo m n
  | m == 1 = False
  | n == 2 = isDiv m n || _primo m 3
  | isqrt m >= n = isDiv m n || _primo m (n + 2)
  | otherwise = False

primo :: (Num t, Integral t) => t -> Bool
primo m = not (_primo m 2)

_seriePI :: (Integral t1, Fractional t2) => t1 -> t1 -> t1 -> t2
_seriePI n m a
  | m <= n = fromIntegral a * (4 / fromIntegral m) + _seriePI n (m + 2) (-1 * a)
  | otherwise = 0

seriePI :: (Integral t, Fractional t1) => t -> t1
seriePI n = _seriePI n 1 1
