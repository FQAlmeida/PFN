module TestLista3 where

import qualified Data.HashTable.IO as H
import Listas.Lista3
import Test.Hspec (describe, hspec, it, shouldBe)

mainTestLista3 :: IO ()
mainTestLista3 = hspec $ do
  describe "linhas" $ do
    it "should separate the document into lines" $ do
      let doc = "Hello\nWorld\n"
      linhas doc `shouldBe` ["Hello", "World"]

  describe "enumerarLinhas" $ do
    it "should number the lines of the document" $ do
      let linhasDoc = ["Hello", "World"]
      enumerarLinhas linhasDoc `shouldBe` [(1, "Hello"), (2, "World")]

  describe "cleanPalavra" $ do
    it "should clean the word by removing punctuation and converting to lowercase" $ do
      let palavra = "Hello, World!"
      cleanPalavra palavra `shouldBe` "hello world"

  describe "palavras" $ do
    it "should extract words from a line, removing punctuation and words with less than 3 letters" $ do
      let linha = "Hello, World! This is a test."
      palavras linha `shouldBe` ["hello", "world", "this", "test"]

  describe "numeraPalavras" $ do
    it "should associate each word occurrence with the line number" $ do
      let linhasEnumeradas = [(1, "Hello"), (2, "World")]
      numeraPalavras linhasEnumeradas `shouldBe` [(1, "hello"), (2, "world")]

  describe "ordenarPalavras" $ do
    it "should sort the word occurrences alphabetically" $ do
      let palavrasOrdenadas = [(2, "world"), (1, "hello")]
      ordenarPalavras palavrasOrdenadas `shouldBe` [(1, "hello"), (2, "world")]

  describe "agrupar" $ do
    it "should group the word occurrences by word, combining the line numbers" $ do
      let palavrasOrdenadas = [(1, "hello"), (2, "hello"), (3, "world")]
      agrupar palavrasOrdenadas `shouldBe` [([1, 2], "hello"), ([3], "world")]

  describe "eliminarRep" $ do
    it "should remove duplicate line numbers from the word occurrences" $ do
      let palavrasAgrupadas = [([1, 2], "hello"), ([2, 3], "world")]
      eliminarRep palavrasAgrupadas `shouldBe` [([1, 2], "hello"), ([2, 3], "world")]

  describe "construirIndice" $ do
    it "should construct the index of word occurrences in the document" $ do
      let doc = "Hello, World!\nHello, Haskell!"
      indice <- construirIndice doc
      indice `shouldBe` [([2], "haskell"), ([1, 2], "hello"), ([1], "world")]

  describe "construirIndiceHashMap" $ do
    let doc = "Hello, World!\nHello, Haskell!"
    it "should construct the index of word occurrences in the document using a hash map" $ do
      indices <- construirIndiceHashMap doc
      hello <- H.lookup indices "hello"
      world <- H.lookup indices "world"
      haskell <- H.lookup indices "haskell"
      hello `shouldBe` Just [1, 2]
      world `shouldBe` Just [1]
      haskell `shouldBe` Just [2]
    it "should return Nothing for words that do not exist in the document" $ do
      indices <- construirIndiceHashMap doc
      unexistent_word <- H.lookup indices "boo"
      unexistent_word `shouldBe` Nothing

  describe "printIndices" $ do
    it "should print the indices of word occurrences in the document" $ do
      indices <- H.fromList [("hello", [1, 2]), ("world", [2]), ("haskell", [1])]
      fmt_indices <- printIndices indices
      fmt_indices `shouldBe` "{\n\t\"haskell\": [1],\n\t\"hello\": [1,2],\n\t\"world\": [2],\n}"