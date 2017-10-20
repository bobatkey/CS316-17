module Lec10 where

import Prelude hiding (Monoid, Foldable (..), Functor(..), all, Maybe (..))

{-    Lecture 10 : FUNCTORS AND CONTAINERS -}

{-

-- data [a] = [] | a :: [a]

-}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

data Maybe a
  = Nothing
  | Just a
  deriving Show

{-    PART I : FUNCTORS -}

{-       map :: (a -> b) -> [a] -> [b]

   In Exercise 3.10, you are asked to define a map operation for
   Trees, which has this type:

      mapTree :: (a -> b) -> Tree a -> Tree b

   These functions both a do similar thing: they take a function 'f',
   some structure containing values of type 'a', and return the *same*
   structure, but this time containing values of type 'b'. We can see
   this graphically. 'map' works on lists:

          [ a1, a2, ..., an ]
            |   |        |
            v   v        v
          [ b1, b2, ..., bn ]

   where b1 == f a1, b2 == f a2, ..., bn = f an.

   Similarly, for trees, we have, for example:

      Node (Node Leaf a1 Leaf) a2 (Node Leaf a3 Leaf)
                      |        |             |
                      v        v             v
      Node (Node Leaf b1 Leaf) b2 (Node Leaf b3 Leaf)

   where, again, b1 == f a1, and so on. -}


class Functor c where
  fmap :: (a -> b) -> c a -> c b

instance Functor Tree where
  fmap f Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Functor [] where
  fmap = map

instance Functor Maybe where
  fmap f Nothing  = Nothing
  fmap f (Just a) = Just (f a)

-- laws:
-- fmap id x == x
-- fmap g (fmap f x) == fmap (g . f) x

data Process = End | Output Bool Process | Input Process Process deriving Show

process2 :: Process -> [Bool] -> Maybe ([Bool], [Bool])
process2 End           bs     = Just ([], bs)
process2 (Output b p)  bs     = fmap (\ (os', ls') -> (b:os', ls'))
                                     (process2 p bs)
                                    
process2 (Input tp fp) []     = Nothing
process2 (Input tp fp) (b:bs) = process2 (if b then tp else fp) bs



{-    PART II : FOLDABLE -}

concLists :: [[a]] -> [a]
concLists [] = []
concLists (xs : xss) = xs ++ concLists xss

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

allList :: [Bool] -> Bool
allList [] = True
allList (x:xs) = x && allList xs


class Monoid m where
  base :: m            -- mempty
  op   :: m -> m -> m  -- mappend

-- law x `op` (y `op` z) == (x `op` y) `op z
--     base `op` x == x
--     x `op` base == x

instance Monoid [x] where
  base = []
  op   = (++)

instance Monoid Int where
  base = 0
  op   = (+)

instance Monoid Bool where
  base = True
  op   = (&&)

instance Monoid Double where
  base = 1
  op   = (*)

foldList :: Monoid m => [m] -> m
foldList [] = base
foldList (x:xs) = x `op` foldList xs

foldTree :: Monoid m => Tree m -> m
foldTree Leaf = base
foldTree (Node l x r) = foldTree l `op` (x `op` foldTree r)

foldMaybe :: Monoid m => Maybe m -> m
foldMaybe Nothing  = base
foldMaybe (Just x) = x

class Foldable c where
  fold :: Monoid m => c m -> m

instance Foldable [] where
  fold = foldList

instance Foldable Tree where
  fold = foldTree

instance Foldable Maybe where
  fold = foldMaybe

sum :: Foldable c => c Int -> Int
sum = fold

size :: (Functor c, Foldable c) => c a -> Int
size = fold . fmap (\ _ -> 1)

all :: (Functor c, Foldable c) => (a -> Bool) -> c a -> Bool
all p = fold . (fmap p)

newtype Any = MkAny Bool

unAny :: Any -> Bool
unAny (MkAny x) = x

instance Monoid Any where
  base = MkAny False
  (MkAny x) `op` (MkAny y) = MkAny (x || y) 

any :: (Functor c, Foldable c) => (a -> Bool) -> c a -> Bool
any p = unAny . fold . (fmap (MkAny . p))

-- fold . fmap f is called foldMap
