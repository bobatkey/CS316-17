module Lec08 where

import Test.QuickCheck

{-   LECTURE 08: QUICKCHECK -}

{-   PART I : WRITING INDIVIDUAL TEST CASES -}

-- artisanal testing, one at a time

append_test_1 :: Bool
append_test_1 =
  [1,2,3] ++ [4,5,6] == [1,2,3,4,5,6]

append_test_2 :: Bool
append_test_2 =
  [4,5,6] ++ [1,2,3] == [4,5,6,1,2,3]

append_test_3 :: Bool
append_test_3 =
  [] ++ [1,2,3] == [1,2,3]

append_test_4 :: Bool
append_test_4 =
  [1,2,3] ++ [] == [1,2,3]

append_tests :: Bool
append_tests =
  and [ append_test_1
      , append_test_2
      , append_test_3
      , append_test_4
      ]

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insert x ys

insert_test_1 :: Bool
insert_test_1 =
  insert 3 [1,2,4,5] == [1,2,3,4,5]



{-   PART II : PROPERTY BASED TESTING WITH QUICKCHECK -}

-- http://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf

-- Why not test with lots of examples, not just one?

append_left_nil_prop :: [Int] -> Bool
append_left_nil_prop xs =
  [] ++ xs == xs

append_right_nil_prop :: [Int] -> Bool
append_right_nil_prop xs =
  xs ++ [] == xs

append_faulty_prop :: [Int] -> Bool
append_faulty_prop xs =
  xs ++ [0] == xs

-- (x + y) + z = x + (y + z)

append_assoc :: [Int] -> [Int] -> [Int] -> Bool
append_assoc xs ys zs =
  (xs ++ ys) ++ zs == xs ++ (ys ++ zs)

reverse_reverse_prop :: [Int] -> Bool
reverse_reverse_prop xs =
  reverse (reverse xs) == xs

reverse_does_nothing :: [Int] -> Bool
reverse_does_nothing xs =
  reverse xs == xs

reverse_append :: [Int] -> [Int] -> Bool
reverse_append xs ys =
  reverse (xs ++ ys) == reverse ys ++ reverse xs

slow_reverse :: [a] -> [a]
slow_reverse [] = []
slow_reverse (x:xs) = slow_reverse xs ++ [x]

reverse_eq_slow_reverse :: [Int] -> Bool
reverse_eq_slow_reverse xs =
  reverse xs == slow_reverse xs

----------------------------------------------------------------------
isSorted :: Ord a => [a] -> Bool
isSorted []       = True
isSorted [x]      = True
isSorted (x:y:ys) = x <= y && isSorted (y:ys)

insert_preserves_sortedness :: Int -> [Int] -> Bool
insert_preserves_sortedness x xs =
  isSorted (insert x (makeSorted 0 xs))

makeSorted :: Int -> [Int] -> [Int]
makeSorted i [] = []
makeSorted i (x:xs) = y : makeSorted y xs
  where y = i + abs x

makeSorted_prop :: [Int] -> Bool
makeSorted_prop xs =
  isSorted (makeSorted 0 xs)

----------------------------------------------------------------------
data CS316Staff = Bob | Fred | Ben deriving Show

instance Arbitrary CS316Staff where
  arbitrary = frequency [ (1, return Bob)
                        , (1, return Fred)
                        , (3, return Ben) ]

heightRank :: CS316Staff -> Int
heightRank Bob  = 3
heightRank Fred = 2
heightRank Ben  = 1

heightRank_prop :: CS316Staff -> Bool
heightRank_prop staffMember =
  heightRank staffMember <= heightRank Bob
  
----------------------------------------------------------------------
data Tree a
  = TLeaf
  | TNode (Tree a) a (Tree a)
  deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = genTree 3

genTree :: Arbitrary a => Int -> Gen (Tree a)
genTree 0 = return TLeaf
genTree n = frequency [ (3, do l <- genTree (n-1)
                               x <- arbitrary
                               r <- genTree (n-1)
                               return (TNode l x r))
                      , (1, return TLeaf)
                      ]


