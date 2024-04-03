module Main where

import qualified Aulas.Aula6 (taylorSerie)

main :: IO ()
main = do
  print (Aulas.Aula6.taylorSerie 10.0 5)
