module Aulas.Aula20 where

data Point = Point Float Float deriving (Show)

(|-|) :: Point -> Point -> Point
Point a b |-| Point c d = Point (a - c) (b - d)

data Shape = Circle Point Float | Rectangle Point Point

-- Também pode ser
-- data Shape = ... deriving Show
-- Mas não há controle do Display de Shape
instance Show Shape where
  show (Circle p r) = "Circle(" ++ show p ++ " " ++ show r ++ ")"
  show (Rectangle p1 p2) = "Rectangle(" ++ show p1 ++ " " ++ show p2 ++ ")"

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ (2 :: Integer)
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

generateRandomShape :: (Integral a) => a -> Shape
generateRandomShape i
  | even i = Circle (Point 10 20) 10
  | otherwise = Rectangle (Point 10 20) (Point 30 40)

generateRandomShapes :: Int -> [Shape]
generateRandomShapes n = map generateRandomShape [0 .. n]

figuras :: [Shape]
figuras = generateRandomShapes 4

-- 1) Implemente a função que calcula a área de uma lista de formas.
areaFiguras :: [Shape] -> [Float]
areaFiguras = map surface

sumAreaFiguras :: [Shape] -> Float
sumAreaFiguras shapes = sum $ areaFiguras shapes

type Name = String

type PhoneNumber = String

type Register = (Name, PhoneNumber)

type Agenda = [Register]

nomes :: Agenda -> [Name]
nomes = map fst

addRegister :: Register -> Agenda -> Agenda
addRegister r a = r : a

createRegister :: Name -> PhoneNumber -> Register
createRegister n p = (n, p)

addRegister' :: Name -> PhoneNumber -> Agenda -> Agenda
addRegister' n p a = createRegister n p : a
