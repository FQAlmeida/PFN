module Aulas.Aula12 where

-- | This module contains code related to the topic of polymorphism.

-- (1) Define a function productList :: [Int] → Int which returns the product of a list
-- of integers. You should take the product of the empty list to be 1.

productList :: Num a => [a] -> a
productList = product

-- (2) Define a function myand :: [Bool ] → Bool which returns the conjunction of a
-- list. Informally,
-- myand [e1, e2, . . . , ei] = e1 && e2 && . . . && ei.
-- The conjunction of an empty list should be True.

myand :: [Bool] -> Bool
myand = and

-- (3) Define a function concatList :: [[Int]] → [Int] which flattens a list of lists of
-- integers into a single list of integers. For example,
-- concatList [[3, 4], [], [31, 3]] = [3, 4, 31, 3].
-- Informally,
-- concatList [e1, e2, . . . , ei] = e1 ++ e2 ++ . . . ++ ei.

concatList :: [[a]] -> [a]
concatList = concat

-- (4) Define the function while which is such that while pred xs returns the longest
-- initial segment of the list xs all of whose elements satisfy the Boolean-valued
-- function pred . For example,
-- while even [2, 4, 8, 3, 4, 8, 6] = [2, 4, 8].

while :: (a -> Bool) -> [a] -> [a]
while predicate (x : xs)
  | predicate x = x : while predicate xs
  | otherwise = []
while _ [] = []

-- (5) The function iSort (insertion sort) is defined as follows:
-- iSort :: [Int] -> [Int]
-- iSort [] = []
-- iSort (x:xs) = ins x (iSort xs)
-- ins :: Int -> [Int] -> [Int]
-- ins x [] = [x]
-- ins x (y:ys)

-- | x <= y = x:y:ys
-- | otherwise = y:ins x ys
-- Use the function iSort to define two functions, minList and maxList, which find
-- the minimum and maximum elements of a non-empty list of integers.
iSort :: [Int] -> [Int]
iSort = foldr ins []

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : ins x ys

minList :: [Int] -> Int
minList lista = minValue
  where
    sortedList = iSort lista
    minValue = head sortedList

maxList :: [Int] -> Int
maxList lista = maxValue
  where
    sortedList = iSort lista
    maxValue = last sortedList

-- (6) Define the functions minList and maxList, which return the minimum and max-
-- imum elements of a non-empty list of integers, respectively, without using iSort
-- or any other sorting function.

_minList :: [Int] -> Int
_minList [x] = x
_minList (x:xs)
    | x < minValue = x
    | otherwise = minValue
    where
        minValue = minList xs
_minList [] = error "Empty list"

_maxList :: [Int] -> Int
_maxList [x] = x
_maxList (x:xs)
    | x > maxValue = x
    | otherwise = maxValue
    where
        maxValue = maxList xs
_maxList [] = error "Empty list"


-- (7) Using the function iSort defined in question (5) redefine the function ins so that
-- the list is sorted in descending order.

_ins :: Int -> [Int] -> [Int]
_ins x [] = [x]
_ins x (y : ys)
  | x >= y = x : y : ys
  | otherwise = y : ins x ys

-- (8) Using the function iSort defined in question (5) redefine the function ins so that,
-- in addition to outputting a list in ascending order, duplicates are removed. For
-- example, iSort [2, 1, 4, 1, 2] = [1, 2, 4].

concatDeduplicated :: Eq a => a -> [a] -> [a]
concatDeduplicated x lista
    | x `elem` lista = lista
    | otherwise = x:lista

concatDeduplicatedDup :: Eq a => a -> a -> [a] -> [a]
concatDeduplicatedDup x y lista = concatDeduplicated x (concatDeduplicated y lista)

-- TODO (Otavio): FIX, it is not removing all duplicates
__ins :: Int -> [Int] -> [Int]
__ins x [] = [x]
__ins x (y : ys)
  | x <= y = concatDeduplicatedDup x y ys
  | otherwise = concatDeduplicated y (ins x ys)

-- (9) Define the function memberNum :: [Int] → Int → Int such that memberNum xs x
-- returns the number of times that x occurs in the list xs. For example,
-- memberNum [2, 1, 4, 1, 2] 2 = 2.

memberNum :: (Eq t, Num a) => [t] -> t -> a
memberNum (x : xs) y
  | x == y = 1 + memberNum xs y
  | otherwise = memberNum xs y
memberNum [] _ = 0

-- (10) The function member :: [Int] → Int → Bool has the property that member xs x
-- returns True if x occurs in the list xs and it returns False if x does not occur
-- in the list xs. Give a definition of member which uses the function memberNum
-- that you defined as the answer to question (9).

member :: Eq t => [t] -> t -> Bool
member xs x = memberNum xs x > (0::Integer)


-- (11) Redefine the function member of question (10) so that it no longer makes use of
-- memberNum (from question (9)).

_member :: Eq t => [t] -> t -> Bool
_member (x : xs) y
  | x == y = True
  | otherwise = member xs y
_member [] _ = False

-- (12) Using pattern matching with : (cons), define a function rev2 that reverses all
-- lists of length 2, but leaves all other lists unchanged.

-- (13) Define a function position which takes a number i and a list of numbers xs and
-- returns the position of i in the list xs, counting the first position as 1. If i does
-- not occur in xs, then position returns 0.
-- (14) Define a function element which takes a list xs and a positive integer i and
-- returns the ith member of xs. Assume that the list xs is at least of length i.
-- (15) Define a function segments which takes a finite list xs as its argument and returns
-- the list of all the segments of xs. (A segment of xs is a selection of adjacent
-- elements of xs.) For example, segments [1, 2, 3] = [[1, 2, 3], [1, 2], [2, 3], [1], [2], [3]].
-- (16) A partition of a positive integer n is a representation of n as the sum of any
-- number of positive integral parts. For example, there are 7 partitions of the
-- number 5: 1 + 1 + 1 + 1 + 1, 1 + 1 + 1 + 2, 1 + 1 + 3, 1 + 2 + 2, 1 + 4, 2 + 3 and 5.
-- Define a function parts which returns the list of distinct partitions of an integer
-- n. For example, parts 4 = [[1, 1, 1, 1], [1, 1, 2], [1, 3], [2, 2], [4]].
-- (17) A segment ys of a list xs is said to be flat if all the elements of ys are equal.
-- Define llfs such that llfs xs is the length of the longest flat segment of xs.
-- (18) A list of numbers is said to be steep if each element of the list is at least as large
-- as the sum of the preceding elements. Define a function llsg such that llsg xs is
-- the length of the longest steep segment of xs.
-- (19) Define a function llsq such that llsq xs is the length of the longest steep subse-
-- quence of xs.
-- (20) Given a sequence of positive and negative integers define a function msg which
-- returns the minimum of the sums of all the possible segments of its argument.
