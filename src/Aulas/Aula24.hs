module Aulas.Aula24 where

data ArvoreBin a = Leaf | Node a (ArvoreBin a) (ArvoreBin a) deriving (Show, Eq)

type ArvoreBinInteiros = ArvoreBin Int

allTreeValues :: (Ord a) => ArvoreBin a -> [a]
allTreeValues Leaf = []
allTreeValues (Node value left right) = allTreeValues left ++ [value] ++ allTreeValues right

allLeafValues :: (Ord a) => ArvoreBin a -> [a]
allLeafValues Leaf = []
allLeafValues (Node value Leaf Leaf) = [value]
allLeafValues (Node _ left right) = allLeafValues left ++ allLeafValues right

allNodeValues :: (Ord a) => ArvoreBin a -> [a]
allNodeValues Leaf = []
allNodeValues (Node _ Leaf Leaf) = []
allNodeValues (Node value left right) = allNodeValues left ++ [value] ++ allNodeValues right

insertNodeValue :: (Ord a) => ArvoreBin a -> a -> ArvoreBin a
insertNodeValue Leaf value = Node value Leaf Leaf
insertNodeValue (Node v l r) value
  | value < v = Node v (insertNodeValue l value) r
  | value > v = Node v l (insertNodeValue r value)
  | otherwise = Node v l r

updateNodeValue :: (Ord a) => ArvoreBin a -> a -> a -> ArvoreBin a
updateNodeValue Leaf _ _ = Leaf
updateNodeValue tree oldValue newValue
  | tree /= trimmedTree = insertNodeValue trimmedTree newValue
  | otherwise = tree
  where
    trimmedTree = removeNodeValue tree oldValue

removeNodeValue :: (Ord a) => ArvoreBin a -> a -> ArvoreBin a
removeNodeValue Leaf _ = Leaf
removeNodeValue (Node v l r) value
  | value < v = Node v (removeNodeValue l value) r
  | value > v = Node v l (removeNodeValue r value)
  | otherwise = removeNode (Node v l r)
  where
    removeNode Leaf = Leaf
    removeNode (Node _ Leaf right) = right
    removeNode (Node _ left Leaf) = left
    removeNode (Node _ left right) = Node minRight left (removeNodeValue right minRight)
      where
        minRight = findMin right
        findMin (Node val Leaf _) = val
        findMin (Node _ lf _) = findMin lf
        findMin Leaf = error "Empty tree"

fromList :: (Ord a) => [a] -> ArvoreBin a
fromList (x : xs) = foldl insertNodeValue (Node x Leaf Leaf) xs
fromList [] = Leaf
