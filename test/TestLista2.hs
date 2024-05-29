{-# OPTIONS_GHC -Wno-type-defaults #-}

module TestLista2 where

import Control.Exception (evaluate)
import Listas.Lista2
  ( binDec,
    decBin,
    enesimo,
    insereOrd,
    intercalacao,
    interseccao,
    inverso,
    inversoDupla,
    menor,
    minusculas,
    nUltimos,
    numString,
    ordenar,
    pertence,
    pot2,
    removeTab,
    removerElem,
    repetir,
    simetrico,
    soma2,
    stringNum,
    trocoCafe,
  )
import Test.Hspec
  ( anyException,
    describe,
    hspec,
    it,
    shouldBe,
    shouldThrow,
  )

mainTestLista2 :: IO ()
mainTestLista2 = hspec $ do
  describe "pertence" $ do
    it "returns True if the element is in the list" $
      pertence 3 [1, 4, 3, 2] `shouldBe` True
    it "returns False if the element is not in the list" $
      pertence 5 [1, 4, 3, 2] `shouldBe` False

  describe "interseccao" $ do
    it "returns the intersection of two lists" $
      interseccao [1, 3, 5, 7, 9] [2, 5, 3, 6, 9] `shouldBe` [3, 5, 9]
    it "returns an empty list if there is no intersection" $
      interseccao [1, 2, 3] [4, 5, 6] `shouldBe` []

  describe "inverso" $ do
    it "returns the reverse of a list" $
      inverso [1, 2, 3, 4] `shouldBe` [4, 3, 2, 1]
    it "returns an empty list if the input list is empty" $
      inverso ([] :: [Int]) `shouldBe` ([] :: [Int])

  describe "nUltimos" $ do
    it "returns the last n elements of a list" $
      nUltimos 3 [1, 2, 3, 4, 5, 6] `shouldBe` [4, 5, 6]
    it "returns the whole list if n is greater than or equal to the length of the list" $
      nUltimos 10 [1, 2, 3] `shouldBe` [1, 2, 3]

  describe "soma2" $ do
    it "returns a list with the sum of corresponding elements from two lists" $
      soma2 [1, 2, 3, 4] [10, 20, 30] `shouldBe` [11, 22, 33]
    it "returns an empty list if one of the input lists is empty" $
      soma2 [1, 2, 3] [] `shouldBe` []

  describe "pot2" $ do
    it "returns a list with the powers of 2 up to 2^n" $
      pot2 4 `shouldBe` [2, 4, 8, 16]
    it "returns an empty list if n is less than 1" $
      pot2 0 `shouldBe` []

  describe "intercalacao" $ do
    it "returns a sorted list by merging two sorted lists" $
      intercalacao [10, 15, 17, 20] [1, 2, 13, 15, 22] `shouldBe` [1, 2, 10, 13, 15, 15, 17, 20, 22]
    it "returns the first list if the second list is empty" $
      intercalacao [1, 2, 3] [] `shouldBe` [1, 2, 3]

  describe "menor" $ do
    it "returns the smallest element in a list" $
      menor [10, 3, 5, 2, 20] `shouldBe` 2
    it "throws an error if the list is empty" $
      evaluate (menor ([] :: [Int])) `shouldThrow` anyException

  describe "removerElem" $ do
    it "returns the list without the first occurrence of the element" $
      removerElem 1 [2, 4, 1, 3, 2, 1] `shouldBe` [2, 4, 3, 2, 1]
    it "returns the same list if the element is not in the list" $
      removerElem 5 [1, 2, 3, 4] `shouldBe` [1, 2, 3, 4]

  describe "ordenar" $ do
    it "returns a sorted list" $
      ordenar [32, 10, 23, 10, 12, 4] `shouldBe` [4, 10, 10, 12, 23, 32]
    it "returns an empty list if the input list is empty" $
      ordenar ([] :: [Int]) `shouldBe` ([] :: [Int])

  describe "insereOrd" $ do
    it "inserts an element into a sorted list at the correct position" $
      insereOrd 12 [6, 9, 10, 15, 20] `shouldBe` [6, 9, 10, 12, 15, 20]
    it "returns the same list if the element is already in the list" $
      insereOrd 10 [6, 9, 10, 15, 20] `shouldBe` [6, 9, 10, 15, 20]

  describe "enesimo" $ do
    it "returns the nth element of a list" $
      enesimo 3 [10, 20, 30, 40, 50] `shouldBe` 40
    it "throws an error if the index is greater than or equal to the length of the list" $
      evaluate (enesimo 5 [1, 2, 3]) `shouldThrow` anyException

  describe "repetir" $ do
    it "returns a list with n repetitions of an element" $
      repetir 4 10 `shouldBe` [10, 10, 10, 10]
    it "returns an empty list if n is less than 1" $
      repetir 0 10 `shouldBe` []

  describe "removeTab" $ do
    it "replaces all tab characters with spaces in a string" $
      removeTab "1\tTeste" `shouldBe` "1 Teste"
    it "returns an empty string if the input string is empty" $
      removeTab "" `shouldBe` ""

  describe "minusculas" $ do
    it "converts all uppercase letters in a string to lowercase" $
      minusculas "AbCdeF" `shouldBe` "abcdef"
    it "returns an empty string if the input string is empty" $
      minusculas "" `shouldBe` ""

  describe "inversoDupla" $ do
    it "returns a list with the reversed pairs of a list of pairs" $
      inversoDupla [(1, 2), (6, 1), (4, 11)] `shouldBe` [(2, 1), (1, 6), (11, 4)]
    it "returns an empty list if the input list is empty" $
      inversoDupla ([] :: [(Int, Int)]) `shouldBe` ([] :: [(Int, Int)])

  describe "simetrico" $ do
    it "returns a list of booleans indicating if the elements in each pair are equal" $
      simetrico [(1, 2), (4, 4), (3, 2)] `shouldBe` [False, True, False]
    it "returns an empty list if the input list is empty" $
      simetrico ([] :: [(Int, Int)]) `shouldBe` ([] :: [Bool])

  describe "numString" $ do
    it "converts an integer to a string representation" $
      numString 126 `shouldBe` "126"

  describe "stringNum" $ do
    it "converts a string of digits to an integer" $
      stringNum "102" `shouldBe` 102

  describe "decBin" $ do
    it "converts an integer to a binary string representation" $
      decBin 13 `shouldBe` "1101"

  describe "binDec" $ do
    it "converts a binary string to an integer" $
      binDec "1101" `shouldBe` 13

  describe "trocoCafe" $ do
    it "returns a list of tuples representing the coins to give as change" $
      trocoCafe 65 110 `shouldBe` [(20, 2), (5, 1)]
