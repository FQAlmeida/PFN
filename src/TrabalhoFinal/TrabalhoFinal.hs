{-# LANGUAGE TupleSections #-}

module TrabalhoFinal.TrabalhoFinal where

import Data.Char (isAlpha, isDigit, toLower)

type Document = String

type Line = String

type Word' = String

data Tree = Node Word' [Int] Tree Tree | Leaf deriving (Show)

readDocument :: FilePath -> IO Document
readDocument filepath = do
  readFile filepath

-- O problema de gerar os índices pode ser dividido nos seguintes subproblemas:
-- a) Separar o documento em Lines: lines :: Doc → [Line]
splitLines :: Document -> [Line]
splitLines = lines

-- b) Numerar as Lines do documento: numLines :: [Line] → [(Int, Line)]
enumerateLines :: [Line] -> [(Int, Line)]
enumerateLines = zip [1 ..]

-- c) Associar  a  cada  ocorrência  de  uma  Word  do  documento,  o  número  da  Line  em  que  essa
-- Word ocorre: numeraWords :: [(Int, Line)] → [(Int, Word)]
-- Antes  de  separar  cada  Line  do  seu  texto  em  Words,  devem  ser  eliminados  da  Line  os
-- caracteres de pontuação, os quais não devem ser incluídos no índice (pesquisar as funções
-- isAlpha, isSpace, isDigit, zip). Words com menos de 3 letras também devem ser eliminadas.
cleanWord :: Word' -> Word'
cleanWord word = map toLower $ filter (\c -> isAlpha c || isDigit c) word

splitWords :: Line -> [Word']
splitWords line = filter (\p -> length p > 3) $ map cleanWord $ words line

enumerateWords :: [(Int, Line)] -> [(Int, Word')]
enumerateWords = concatMap (\(line_number, line) -> map (line_number,) (splitWords line))

-- d) Inserir elementos em uma lista ordenada, o elemento deve ser inserido em uma posição que
-- mantenha a lista resultante ordenada. Caso a lista já contenha o elemento não deve ocorrer a
-- inserção:
insertInOrder :: (Ord a) => a -> [a] -> [a]
insertInOrder a [] = [a]
insertInOrder a (x : xs)
  | a == x = x : xs
  | a < x = a : x : xs
  | otherwise = x : insertInOrder a xs

-- e) Inserir uma Word e Line de ocorrência na árvore, caso a Word já tenha sido inserida
-- apenas a Line deve ser adicionada a lista de Lines relacionadas com a Word. Deve ser usada
-- a função definida no item anterior para essa tarefa:
insertWordInTree :: (Int, Word') -> Tree -> Tree
insertWordInTree (line, word) Leaf = Node word [line] Leaf Leaf
insertWordInTree (line, word) (Node node_word lines' left right)
  | word == node_word = Node node_word (insertInOrder line lines') left right
  | word < node_word = Node node_word lines' (insertWordInTree (line, word) left) right
  | otherwise = Node node_word lines' left (insertWordInTree (line, word) right)

-- f) Percorrer a lista com as duplas de Words e Lines inserido cada uma delas na árvore:
createTreeFromWords :: [(Int, Word')] -> Tree
createTreeFromWords = foldr insertWordInTree Leaf

buildIndex :: Document -> Tree
buildIndex doc = createTreeFromWords enumeratedWords
  where
    enumeratedWords = enumerateWords enumeratedLines
    enumeratedLines = enumerateLines $ splitLines doc

padding :: Int -> String
padding depth = "|" ++ replicate (depth * 2) '-'

showTree :: Tree -> Int -> String
showTree Leaf _ = ""
showTree (Node word lines' left right) depth = thisNode ++ children
  where
    thisNode = padding depth ++ show (depth + 1) ++ ": " ++ word ++ " " ++ show lines' ++ "\n"
    children = leftChild ++ rightChild
    leftChild = showTree left (depth + 1)
    rightChild = showTree right (depth + 1)

mainTrabalhoFinal :: IO ()
mainTrabalhoFinal = do
  doc <- readDocument "data/alice_haskell.txt"
  let index = buildIndex doc
  putStrLn "|---------------------------------------------------------------------------------"
  putStr $ showTree index 0
  putStrLn "|---------------------------------------------------------------------------------"
