{-# OPTIONS_GHC -Wno-type-defaults #-}

module TestLista1 where

import Listas.Lista1
  ( ehTriangulo,
    isDiv,
    mapEvenIndex,
    primo,
    seriePI,
    somaPares,
    somaPot2m,
    tipoTriangulo,
    triangulo,
    __seriePI,
  )
import Test.Hspec (describe, hspec, it, shouldBe)

mainTestLista1 :: IO ()
mainTestLista1 = hspec $ do
  describe "ehTriangulo" $ do
    it "returns True for valid triangle sides" $ do
      ehTriangulo 3 4 5 `shouldBe` True
      ehTriangulo 5 12 13 `shouldBe` True
    it "returns False for invalid triangle sides" $ do
      ehTriangulo 1 2 5 `shouldBe` False
      ehTriangulo 5 10 25 `shouldBe` False

  describe "tipoTriangulo" $ do
    it "returns 'equilatero' for equilateral triangles" $ do
      tipoTriangulo 5 5 5 `shouldBe` "equilatero"
    it "returns 'isosceles' for isosceles triangles" $ do
      tipoTriangulo 5 5 8 `shouldBe` "isosceles"
      tipoTriangulo 5 8 5 `shouldBe` "isosceles"
      tipoTriangulo 8 5 5 `shouldBe` "isosceles"
    it "returns 'escaleno' for scalene triangles" $ do
      tipoTriangulo 3 4 5 `shouldBe` "escaleno"
      tipoTriangulo 5 12 13 `shouldBe` "escaleno"

  describe "triangulo" $ do
    it "returns the type of triangle if it is valid" $ do
      triangulo 3 4 5 `shouldBe` "escaleno"
      triangulo 5 5 5 `shouldBe` "equilatero"
    it "returns 'nao eh um triangulo' if it is not a valid triangle" $ do
      triangulo 1 2 5 `shouldBe` "nao eh um triangulo"
      triangulo 5 10 25 `shouldBe` "nao eh um triangulo"

  describe "somaPares" $ do
    it "returns the sum of even numbers from 0 to n" $ do
      somaPares 10 `shouldBe` 30
      somaPares 20 `shouldBe` 110

  describe "somaPot2m" $ do
    it "returns the sum of powers of 2 multiplied by m" $ do
      somaPot2m 2 3 `shouldBe` 30
      somaPot2m 3 4 `shouldBe` 93

  describe "isDiv" $ do
    it "returns True if num is divisible by divisor" $ do
      isDiv 10 2 `shouldBe` True
      isDiv 15 3 `shouldBe` True
    it "returns False if num is not divisible by divisor" $ do
      isDiv 10 3 `shouldBe` False
      isDiv 15 4 `shouldBe` False

  describe "primo" $ do
    it "returns True if the number is prime" $ do
      primo 2 `shouldBe` True
      primo 7 `shouldBe` True
    it "returns False if the number is not prime" $ do
      primo 1 `shouldBe` False
      primo 4 `shouldBe` False

  describe "seriePI" $ do
    it "returns the sum of the series for a given number of terms" $ do
      seriePI 10 `shouldBe` 3.33968253968254
      seriePI 100 `shouldBe` 3.1215946525910105

  describe "mapEvenIndex" $ do
    it "applies a function to elements at even indices in a list" $ do
      mapEvenIndex (+ 1) [1, 2, 3, 4, 5, 6] `shouldBe` [1, 3, 3, 5, 5, 7]
      mapEvenIndex (* 2) [1, 2, 3, 4, 5, 6] `shouldBe` [1, 4, 3, 8, 5, 12]

  describe "__seriePI" $ do
    it "returns the sum of the series for a given number of terms" $ do
      __seriePI 10 `shouldBe` 2.9760461760461765
      __seriePI 100 `shouldBe` 3.1611986129870506
      __seriePI 1000000 `shouldBe` 3.1415946535856922
