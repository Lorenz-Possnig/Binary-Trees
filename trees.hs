{-
    Trees by Lorenz Possnig

This library contains the Tree type representing binary search trees
                as well as some usefull funcitons

            Nodes are always traversed in LVR order

-}

module BinaryTrees where

data Tree a = EmptyTree
    | Node {-# UNPACK #-} !(Tree a)
           {-# UNPACK #-} !a
           {-# UNPACK #-} !(Tree a)
    deriving (Show, Eq, Read)

--Creates a tree consisting of one value and two empty subtrees
singleton :: a -> Tree a
singleton a = Node EmptyTree a EmptyTree

--Creates a new tree consisting of the old tree
--with the new value inserted at the correct position
insert :: (Ord a) =>  a -> Tree a -> Tree a
insert v EmptyTree = singleton v
insert v (Node left key right)
    |v <= key = Node (insert v left) key right
    |otherwise = Node left key (insert v right)

--Like insert but allows the programmer to supply their own comparison function
insertBy :: (Ord a) => (a -> a -> Ordering) -> a -> Tree a -> Tree a
insertBy func val EmptyTree = singleton val
insertBy func val (Node left key right) = case func val key of
    LT -> Node (insertBy func val left) key right
    EQ -> Node (insertBy func val left) key right
    GT -> Node left key (insertBy func val right)

--Converts a linked list into a tree
--Values may appear multiple times
fromList :: (Ord a) => [a] -> Tree a
fromList list = foldl (\x y -> insert y x) EmptyTree list

--Converts a List into a Tree, allowing the programmer to supply a comparison function
--Used by treeSortBy
fromListBy :: (Ord a) => (a -> a -> Ordering) -> [a] -> Tree a
fromListBy func list = foldl (\x y -> insertByFunc y x) EmptyTree list where
    insertByFunc = insertBy func

--Converts a tree back into a list
toList :: (Ord a) => Tree a -> [a]
toList EmptyTree = []
toList (Node left key right) = (toList left) ++ [key] ++ (toList right)

--Sorts a list by turning it into a tree and back into a list
treeSort :: (Ord a) => [a] -> [a]
treeSort list = toList $ fromList list

--Like treeSort, but allows the programmer to supply a comparison function
treeSortBy :: Ord a => (a -> a -> Ordering) -> [a] -> [a]
treeSortBy func list = toList $ fromListBy func list

--Applies a function to every value in the tree
map :: (a -> b) -> Tree a -> Tree b
map func EmptyTree = EmptyTree
map func (Node EmptyTree key EmptyTree) = Node EmptyTree (func key) EmptyTree
map func (Node left key right) = Node (BinaryTrees.map func left) (func key) (BinaryTrees.map func right)

--like fold but with trees
fold :: (a -> b -> b) -> b -> Tree a -> b
fold func def EmptyTree = def
fold func def (Node left key right) = func key (BinaryTrees.fold func (BinaryTrees.fold func def left) right)

--Returns true if val is an element of the tree
elem :: (Ord a) => a -> Tree a -> Bool
elem val EmptyTree = False
elem val (Node left key right)
    |val == key = True
    |val < key = BinaryTrees.elem val left
    |otherwise = BinaryTrees.elem val right

--Returns the height of a tree
height :: Tree a -> Int
height EmptyTree = 0
height (Node left _ right) = 1 + max (height left) (height right)

--Returns the left subtree of a tree 
left :: Tree a -> Tree a
left (Node l _ _) = l

--Returns the right subtree of a tree
right :: Tree a -> Tree a
right (Node _ _ r) = r

--Returns true if node is a leaf -> has two empty subtrees
isLeaf :: Tree a -> Bool
isLeaf node = case node of
    (Node EmptyTree key EmptyTree) -> True
    _ -> False

--Returns true if a tree is empty
null :: Tree a -> Bool
null tree = case tree of
    EmptyTree -> True
    _ -> False

--Returns a string containing a 2D ASCII Drawing of a Tree
-- "+-" represents the right subtree of a node
-- "`-" represents the left subtree of a node
-- "()" represents an empty node
-- Looks bad, but works
drawTree :: Tree String -> String
drawTree = unlines . draw where
    draw :: Tree String -> [String]
    draw EmptyTree = ["()"]
    draw (Node left key right) = lines key ++ drawSubTrees (left:[right]) where
        drawSubTrees :: [Tree String] -> [String]
        drawSubTrees [] = []
        drawSubTrees [a] = "|" : shift "+- " "   " (draw a)
        drawSubTrees (h:t) = "|" : shift "`- " "|  " (draw h) ++ drawSubTrees t
    shift first other = zipWith (++) (first : repeat other)