module Main (main) where

import TestLista1 (mainTestLista1)
import TestLista2 (mainTestLista2)
import TestLista3 (mainTestLista3)

main :: IO ()
main = do
  mainTestLista1
  mainTestLista2
  mainTestLista3
