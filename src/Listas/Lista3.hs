module Listas.Lista3 where

import Data.Char (isAlpha, isDigit, isSpace, toLower)
import Data.List (sortBy)

type Doc = String

type Linha = String

type Palavra = String

readDoc :: IO Doc
readDoc = do
  readFile "data/small_text.txt"

-- O problema de gerar os índices pode ser dividido nos seguintes subproblemas:
--     a) Separar o documento em linhas: lines :: Doc → [Linha]
linhas :: Doc -> [Linha]
linhas = lines

--     b) Numerar as linhas do documento: numLinhas :: [Linha] → [(Int, Linha)]
linhasEnumeradas :: [Linha] -> [(Int, Linha)]
linhasEnumeradas = zip [1 ..]

--     c) Associar  a  cada  ocorrência  de  uma  palavra  do  documento,  o  número  da  linha  em  que  essa
--     palavra ocorre: numeraPalavras :: [(Int, Linha)] → [(Int, Palavra)]
--     Antes  de  separar  cada  linha  do  seu  texto  em  palavras,  devem  ser  eliminados  da  linha  os
--     caracteres de pontuação, os quais não devem ser incluídos no índice (pesquisar as funções
--     isAlpha, isSpace, isDigit, zip). Palavras com menos de 3 letras também devem ser eliminadas.
cleanPalavra :: Palavra -> Palavra
cleanPalavra palavra = map toLower $ filter (\c -> isAlpha c || isDigit c || isSpace c) palavra

palavras :: Linha -> [Palavra]
palavras linha = filter (\p -> length p > 3) $ map cleanPalavra $ words linha

numeraPalavras :: [(Int, Linha)] -> [(Int, Palavra)]
numeraPalavras linhasEnum = [(n, p) | (n, l) <- linhasEnum, p <- palavras l]

--     d) Ordenar alfabeticamente as ocorrências das palavras no texto:
--     ordenar :: [(Int, Palavra)] → [(Int, Palavra)]
ordenarPalavras :: [(Int, Palavra)] -> [(Int, Palavra)]
ordenarPalavras = sortBy (\(_, p1) (_, p2) -> compare p1 p2)

--     e) Juntar  as  várias  ocorrências  de  cada  palavra,  produzindo,  para  cada  palavra,  a  lista  dos
--     números das linhas em que a palavra ocorre:
--     agrupar :: [(Int, Palavra)] → [([Int], Palavra)]
agrupar :: [(Int, Palavra)] -> [([Int], Palavra)]
agrupar ((i, palavra) : xs) = (i : is, palavra) : agrupar xs
  where
    is = map fst $ filter (\(_, p) -> p == palavra) xs
agrupar [] = []

--     f) Eliminar,  da  lista  de  números  de  linhas  em  que  cada  palavra  ocorre,  as  repetições  de  um
--     mesmo número de linha:
--     eliminarRep :: [([Int], Palavra)] → [([Int], Palavra)]
eliminarRep :: [([Int], Palavra)] -> [([Int], Palavra)]
eliminarRep ((i, p) : xs)
  | p `elem` map snd xs = eliminarRep xs
  | otherwise = (i, p) : eliminarRep xs
eliminarRep [] = []

construirIndice :: Doc -> [([Int], Palavra)]
construirIndice doc = do
  let l = linhas doc
  let linhasEnum = linhasEnumeradas l
  let palavrasNum = numeraPalavras linhasEnum
  let palavrasOrdenadas = ordenarPalavras palavrasNum
  let palavrasAgrupadas = agrupar palavrasOrdenadas
  eliminarRep palavrasAgrupadas

mainLista3 :: IO ()
mainLista3 = do
  doc <- readDoc
  let l = linhas doc
  print "---------------------------------------------------------------------------------"
  print l
  print "---------------------------------------------------------------------------------"
  print "---------------------------------------------------------------------------------"
  let linhasEnum = linhasEnumeradas l
  print linhasEnum
  print "---------------------------------------------------------------------------------"
  print "---------------------------------------------------------------------------------"
  let palavrasNum = numeraPalavras linhasEnum
  print palavrasNum
  print "---------------------------------------------------------------------------------"
  print "---------------------------------------------------------------------------------"
  let palavrasOrdenadas = ordenarPalavras palavrasNum
  print palavrasOrdenadas
  print "---------------------------------------------------------------------------------"
  print "---------------------------------------------------------------------------------"
  let palavrasAgrupadas = agrupar palavrasOrdenadas
  print palavrasAgrupadas
  print "---------------------------------------------------------------------------------"
  print "---------------------------------------------------------------------------------"
  let indice = eliminarRep palavrasAgrupadas
  print indice
  print "---------------------------------------------------------------------------------"
