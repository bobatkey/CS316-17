module Lec05 where

{-     LECTURE 05 : RECURSIVE FUNCTIONS  -}

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x <= y then x : y : ys else y : insert x ys

{- insert 3 [1,4]
  =
   1 : insert 3 [4]
  =
   1 : 3 : 4 : []
  =
   [1,3,4]
-}

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

{- isort [3,2,1]
  =   { 4 }
   insert 3 (insert 2 (insert 1 []))
  =   { 1 }
   insert 3 (insert 2 [1])
  =   { 2 }
   insert 3 [1,2]
  =   { 3 }
   [1.2.3]
-}

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where smaller = [ y | y <- xs, y < x]
        larger  = [ y | y <- xs, y >= x]

{-
     qsort [5,3,1,2]
   =
     qsort [3,1,2]                    ++ [5] ++ qsort []
   =
     (qsort [1,2] ++ [3] ++ qsort []) ++ [5] ++ []
   =
     (qsort [1,2]                    ++ [3] ++ qsort []) ++ [5] ++ []
   =
     ((qsort [] ++ [1] ++ qsort [2])        ++ [3] ++ [] ++ [5] ++ []
   =
     (([]       ++ [1] ++ [] ++ [2] ++ [] ) ++ [3] ++ [] ++ [5] ++ []
   = [1,2,3,5]
-}
{-
           [5]
       [3]     []
   [1]
 []  [2]

-}

data BST a
  = Leaf
  | Node (BST a) a (BST a)
  deriving Show

insertBST :: Ord a => a -> BST a -> BST a
insertBST x Leaf = Node Leaf x Leaf
insertBST x (Node smaller y larger)
  | x < y     = Node (insertBST x smaller) y larger
  | otherwise = Node smaller y (insertBST x larger)

listToTree :: Ord a => [a] -> BST a
listToTree [] = Leaf
listToTree (x:xs) = insertBST x (listToTree xs)

flatten :: BST a -> [a]
flatten Leaf = []
flatten (Node smaller a larger) =
  flatten smaller ++ [a] ++ flatten larger

treesort :: Ord a => [a] -> [a]
treesort xs = flatten2 (listToTree xs) []

{-
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : (append xs ys)
-}

flatten2 :: BST a -> [a] -> [a]
flatten2 Leaf acc = acc
flatten2 (Node smaller x larger) acc =
  flatten2 smaller (x:flatten2 larger acc)
