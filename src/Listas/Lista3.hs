module Listas.Lista3 where

import Control.Monad (forM_)
import qualified Data.Bifunctor
import Data.Char (isAlpha, isDigit, isSpace, toLower)
import Data.HashTable.IO (BasicHashTable)
import qualified Data.HashTable.IO as H
import Data.List (group, groupBy, sort, sortBy)

type Doc = String

type Linha = String

type Palavra = String

type HashTable k v = BasicHashTable k v

readDoc :: IO Doc
readDoc = do
  readFile "data/alice_haskell.txt"

-- O problema de gerar os índices pode ser dividido nos seguintes subproblemas:
-- a) Separar o documento em linhas: lines :: Doc → [Linha]
linhas :: Doc -> [Linha]
linhas = lines

-- b) Numerar as linhas do documento: numLinhas :: [Linha] → [(Int, Linha)]
enumerarLinhas :: [Linha] -> [(Int, Linha)]
enumerarLinhas = zip [1 ..]

-- c) Associar  a  cada  ocorrência  de  uma  palavra  do  documento,  o  número  da  linha  em  que  essa
-- palavra ocorre: numeraPalavras :: [(Int, Linha)] → [(Int, Palavra)]
-- Antes  de  separar  cada  linha  do  seu  texto  em  palavras,  devem  ser  eliminados  da  linha  os
-- caracteres de pontuação, os quais não devem ser incluídos no índice (pesquisar as funções
-- isAlpha, isSpace, isDigit, zip). Palavras com menos de 3 letras também devem ser eliminadas.
cleanPalavra :: Palavra -> Palavra
cleanPalavra palavra = map toLower $ filter (\c -> isAlpha c || isDigit c || isSpace c) palavra

palavras :: Linha -> [Palavra]
palavras linha = filter (\p -> length p > 3) $ map cleanPalavra $ words linha

numeraPalavras :: [(Int, Linha)] -> [(Int, Palavra)]
numeraPalavras linhas_enumeradas = [(n, p) | (n, l) <- linhas_enumeradas, p <- palavras l]

-- d) Ordenar alfabeticamente as ocorrências das palavras no texto:
-- ordenar :: [(Int, Palavra)] → [(Int, Palavra)]
ordenarPalavras :: [(Int, Palavra)] -> [(Int, Palavra)]
ordenarPalavras = sortBy (\(_, p1) (_, p2) -> compare p1 p2)

-- e) Juntar  as  várias  ocorrências  de  cada  palavra,  produzindo,  para  cada  palavra,  a  lista  dos
-- números das linhas em que a palavra ocorre:
-- agrupar :: [(Int, Palavra)] → [([Int], Palavra)]
agrupar :: [(Int, Palavra)] -> [([Int], Palavra)]
agrupar palavras_ordenadas = map (\x -> (map fst x, snd (head x))) $ groupBy (\(_, p1) (_, p2) -> p1 == p2) palavras_ordenadas

-- f) Eliminar,  da  lista  de  números  de  linhas  em  que  cada  palavra  ocorre,  as  repetições  de  um
-- mesmo número de linha:
-- eliminarRep :: [([Int], Palavra)] → [([Int], Palavra)]
ddup :: (Ord a) => [a] -> [a]
ddup = map head . group . sort

eliminarRep :: [([Int], Palavra)] -> [([Int], Palavra)]
-- eliminarRep = map (\(is, p) -> (ddup is, p))
eliminarRep = map (Data.Bifunctor.first ddup)

construirIndice :: Doc -> IO [([Int], Palavra)]
construirIndice doc = do
  let linhas_doc = linhas doc
  let linhas_enumeradas = enumerarLinhas linhas_doc
  let palavras_enumeradas = numeraPalavras linhas_enumeradas
  let palavras_ordenadas = ordenarPalavras palavras_enumeradas
  let palavras_agrupadas = agrupar palavras_ordenadas
  let indices = eliminarRep palavras_agrupadas
  return indices

construirIndiceHashMap :: Doc -> IO (HashTable Palavra [Int])
construirIndiceHashMap doc = do
  indices <- construirIndice doc
  ht <- H.newSized $ length indices
  forM_ indices (\(is, p) -> H.insert ht p is)
  return ht

printIndices :: HashTable Palavra [Int] -> IO String
printIndices indices = do
  body <- H.foldM (\acc (k, v) -> return $ acc ++ "\t" ++ show k ++ ": " ++ show v ++ ",\n") "" indices
  let fmt_body = "{\n" ++ body ++ "}"
  return fmt_body

mainLista3 :: IO ()
mainLista3 = do
  doc <- readDoc
  indices <- construirIndiceHashMap doc
  fmt_indices <- printIndices indices
  print "---------------------------------------------------------------------------------"
  print fmt_indices
  print "---------------------------------------------------------------------------------"
