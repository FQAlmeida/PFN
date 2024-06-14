module TestTrabalhoFinal where

import qualified Data.HashTable.IO as H
import TrabalhoFinal.TrabalhoFinal
  ( agrupar,
    cleanPalavra,
    construirIndice,
    construirIndiceHashMap,
    eliminarRep,
    enumerarLinhas,
    linhas,
    numeraPalavras,
    ordenarPalavras,
    palavras,
    printIndices,
  )
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
