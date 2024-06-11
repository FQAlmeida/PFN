module Aulas.Aula24 where

-- Define the binary tree data type
data ArvoreBin a = Leaf | Node a (ArvoreBin a) (ArvoreBin a) deriving Show

-- Define a type alias for binary trees holding only integers
type ArvoreBinInteiros = ArvoreBin Int

-- Example tree creation
exampleTree :: ArvoreBinInteiros
exampleTree =
  Node
    5
    ( Node
        3
        (Node 1 Leaf Leaf)
        (Node 4 Leaf Leaf)
    )
    ( Node
        8
        (Node 6 Leaf Leaf)
        (Node 10 Leaf Leaf)
    )

allTreeValues :: (Ord a) => ArvoreBin a -> [a]
allTreeValues Leaf = []
allTreeValues (Node value left right) = allNodeValues left ++ [value] ++ allNodeValues right

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
updateNodeValue (Node v l r) oldValue newValue
  | oldValue == v = Node newValue l r
  | oldValue < v = Node v (updateNodeValue l oldValue newValue) r
  | otherwise = Node v l (updateNodeValue r oldValue newValue)

removeNodeValue :: Ord a => a -> ArvoreBin a -> ArvoreBin a
removeNodeValue _ Leaf = Leaf
removeNodeValue x (Node val left right)
    | x < val   = Node val (removeNodeValue x left) right
    | x > val   = Node val left (removeNodeValue x right)
    | otherwise = removeNode (Node val left right)
    where
        removeNode :: ArvoreBin a -> ArvoreBin a
        removeNode Leaf = Leaf
        removeNode (Node _ Leaf r) = r
        removeNode (Node _ l Leaf) = l
        removeNode (Node _ l r) = Node minRight l (removeNodeValue minRight r)
            where
                minRight = findMin right
                findMin (Node v Leaf _) = v
                findMin (Node _ lf _) = findMin lf
                findMin Leaf = error "Empty tree"

