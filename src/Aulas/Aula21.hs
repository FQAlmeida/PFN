module Aulas.Aula21 where

import Data.Char (toUpper)
import GHC.IO.Handle (hClose, hGetContents)
import GHC.IO.Handle.FD (openFile, withFile)
import GHC.IO.IOMode (IOMode (ReadMode))

-- Aula 21 - IO e Monads
-- Olhar arquivo Main.hs

askTwoNumbers :: IO (Int, Int)
askTwoNumbers = do
  putStrLn "Digite um número: "
  x1 <- getLine
  let x1' = read x1 :: Int
  putStrLn "Digite outro número: "
  x2 <- getLine
  let x2' = read x2 :: Int
  return (x1', x2')

sumNumbers :: (Num a) => a -> a -> a
sumNumbers x1 x2 = x1 + x2

readFileAlice :: IO ()
readFileAlice = do
  conteudo <- readFile "data/alice_haskell.txt"
  putStrLn conteudo

readFileAliceContents :: IO ()
readFileAliceContents = do
  handle <- openFile "data/alice_haskell.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

readFileAliceWith :: IO ()
readFileAliceWith = do
  withFile
    "data/alice_haskell.txt"
    ReadMode
    ( \handle -> do
        contents <- hGetContents handle
        putStr contents
    )

readFileMapUpper :: IO ()
readFileMapUpper = do
  conteudo <- readFile "data/alice_haskell.txt"
  let teste = map toUpper conteudo
  putStrLn teste

writeFileAlice :: IO ()
writeFileAlice = do
  todoItem <- getLine
  appendFile "data/todo.txt" (todoItem ++ "\n")

aula21Main :: IO ()
aula21Main = do
  readFileAlice
  readFileAliceContents
  readFileAliceWith
  readFileMapUpper
  writeFileAlice
