module TestAula24 where

import Aulas.Aula24 (ArvoreBin (Leaf, Node), allLeafValues, allNodeValues, allTreeValues, fromList, insertNodeValue, removeNodeValue, updateNodeValue)
import Test.Hspec (describe, hspec, it, shouldBe)

mainTestAula24 :: IO ()
mainTestAula24 = hspec $ do
  describe "convert to tree from list" $ do
    it "converts a list to a tree" $ do
      fromList [1, 2, 3, 4, 5, 6]
        `shouldBe` Node
          (1 :: Integer)
          Leaf
          (Node 2 Leaf (Node 3 Leaf (Node 4 Leaf (Node 5 Leaf (Node 6 Leaf Leaf)))))
      fromList [5, 3, 8, 1, 4, 6, 9]
        `shouldBe` Node
          (5 :: Integer)
          (Node 3 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf))
          (Node 8 (Node 6 Leaf Leaf) (Node 9 Leaf Leaf))
  describe "allTreeValues" $ do
    it "returns all values in the tree" $ do
      allTreeValues (Node 1 Leaf Leaf) `shouldBe` [1 :: Integer]
      allTreeValues (Node 1 Leaf (Node 2 Leaf Leaf)) `shouldBe` [1 :: Integer, 2]
      allTreeValues (fromList [1, 2, 3]) `shouldBe` [1 :: Integer, 2, 3]
      allTreeValues (fromList [9, 8, 7]) `shouldBe` [7 :: Integer, 8, 9]
      allTreeValues (fromList [7, 8, 3]) `shouldBe` [3 :: Integer, 7, 8]
      allTreeValues (fromList [1, 2, 3, 4, 5, 6, 7, 8, 9]) `shouldBe` [1 :: Integer, 2, 3, 4, 5, 6, 7, 8, 9]
      allTreeValues (fromList [9, 8, 7, 6, 5, 4, 3, 2, 1]) `shouldBe` [1 :: Integer, 2, 3, 4, 5, 6, 7, 8, 9]
      allTreeValues (fromList [7, 8, 3, 4, 9, 6, 5, 2, 1]) `shouldBe` [1 :: Integer, 2, 3, 4, 5, 6, 7, 8, 9]

  describe "allNodeValues" $ do
    it "returns all node values in the tree" $ do
      allNodeValues (fromList [5, 3, 8, 1, 4, 6, 9]) `shouldBe` [3 :: Integer, 5, 8]
  describe "allLeafNodes" $ do
    it "returns all leaf values in the tree" $ do
      allLeafValues (fromList [5, 3, 8, 1, 4, 6, 9]) `shouldBe` [1 :: Integer, 4, 6, 9]
  describe "insertNodeValue" $ do
    it "inserts a new node in the tree" $ do
      insertNodeValue (fromList [5, 3, 8, 1, 4, 6, 9]) 7
        `shouldBe` Node
          (5 :: Integer)
          (Node 3 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf))
          (Node 8 (Node 6 Leaf (Node 7 Leaf Leaf)) (Node 9 Leaf Leaf))
  describe "updateNodeValue" $ do
    it "updates a node value in the tree" $ do
      updateNodeValue (fromList [5, 3, 8, 1, 4, 6, 9]) 6 10
        `shouldBe` Node
          (5 :: Integer)
          (Node 3 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf))
          (Node 8 Leaf (Node 9 Leaf (Node 10 Leaf Leaf)))
  describe "removeNodeValue" $ do
    it "removes a node value in the tree" $ do
      removeNodeValue (fromList [5, 3, 8, 1, 4, 6, 9]) 8
        `shouldBe` Node
          (5 :: Integer)
          (Node 3 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf))
          (Node 9 (Node 6 Leaf Leaf) Leaf)
