module Lec10 where

import Prelude hiding (Monoid, Foldable (..), Functor(..), all, Maybe (..))

{-     LECTURE 10 : FUNCTORS AND CONTAINERS

   So far, we have looked at several kinds of data structure that
   'contain' some other kind of data. Our most common example has been
   the 'lists'. We've seen the definition of list multiple times so
   far, but here it is again using the standard Haskell notation
   rather than the 'Nil' and 'Cons' constructors.

      data [a] = [] | a :: [a]

   Lists store data in a sequence, one after the other.

   Another example of a container-like structure is a tree: -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{- 'Tree's store data organised into a tree-like shape, with a single
    root and where each piece of data has two 'children'. Trees like
    this are often useful for storing data for quick lookup, when used
    as binary search trees. In Lecture 7 (Declaring Types and
    Classes), we saw some other kinds of tree suitable for storing
    other kinds of data.

    Another kind of container we've seen in this course is 'Maybe'. It
    may not seem sensible at first sight to see of this as a
    container, but we can think of a value of type 'Maybe a' is either
    containing zero or one 'a's. This is similar to how list of 'a's
    can contain zero, one, two, ... 'a's. -}

data Maybe a
  = Nothing
  | Just a
  deriving Show

{- We now have three different kinds of container types: lists, trees,
   and 'Maybe's. This lecture is about what they have in common that
   makes them 'container'-like, and how we express that in Haskell. -}

{-    PART I : FUNCTORS -}

{- In Lecture 6 (Higher Order Functions), we saw the function 'map',
   which applies a function to every element of a list to yield a new
   list of tranformed elements:

      map :: (a -> b) -> [a] -> [b]

   In Exercise 3.10, you are asked to define a map operation for
   'Tree's, which has this type:

      mapTree :: (a -> b) -> Tree a -> Tree b

   These functions both a do similar thing: they take a function 'f',
   some structure containing values of type 'a', and return the *same*
   structure, but this time containing values of type 'b'. We can draw
   this graphically. The function 'map' works on lists:

          [ a1, a2, ..., an ]
            |   |        |
            v   v        v
          [ b1, b2, ..., bn ]

   where b1 = f a1, b2 = f a2, ..., bn = f an.

   Similarly, for trees, we have, for example:

      Node (Node Leaf a1 Leaf) a2 (Node Leaf a3 Leaf)
                      |        |             |
                      v        v             v
      Node (Node Leaf b1 Leaf) b2 (Node Leaf b3 Leaf)

   where, again, b1 == f a1, and so on.

   The important point to see here is that in both cases, mapping does
   not affect the *structure* of the container, only the values stored
   within it. This is an important enough concept that there is a
   special name for it. Type constructors that support an operation
   analogous to 'map' are called "Functors".

   Let's look at the type of the two mapping functions together so we
   can see how to generalise them:

       map     : (a -> b) -> [a]    -> [b]
       mapTree : (a -> b) -> Tree a -> Tree b

   The only place where these differ is the name of the container
   type. For 'map' it is '[]', meaning "lists". For 'mapTree' it is
   'Tree'. Similar in spirit to how we generalised from the specific
   to the general in Lecture 6 (Higher-Order functions), we replace
   the specific '[]' or 'Tree' with a generic 'c' to get:

       fmap   : (a -> b) -> c a -> c b

   We read this as "given a way of transforming 'a's to 'b's, we get a
   way of transforming a 'c' container full of 'a's into a 'c'
   container full of 'b's. If we replace 'c' with '[]' or 'Tree', we
   get the types of 'map' and 'mapTree' above.

   Due to the diversity of different sorts of containers, it isn't
   going to be possible to write one 'fmap' function with this
   type. Instead, we must write a separate one for each kind of
   container. Similar to how each type can have its own 'show'
   function, with a common interface described by the 'Show' typeclass
   (see Lecture 7 (Declaring Types and Classes)), we define a
   typeclass 'Functor' that describes this common interface. (The name
   'Functor' is chosen for historical reasons, one might also call it
   'Mappable'.) -}

class Functor c where
  fmap :: (a -> b) -> c a -> c b

{- As we did for 'Show', we now write "instances" of the 'Functor' type
   class for each of the container types we've seen. For lists, the
   built-in 'map' function does what we want: -}

instance Functor [] where
  fmap = map

{- For 'Tree's, we need to implement 'fmap' ourselves, since it is a
   type we defined ourselves. In Exercise 3.10, we ask you to
   implement 'mapTree' using 'iterTree'. Here, we define it directly
   using pattern matching: -}

instance Functor Tree where
  fmap f Leaf         = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

{- 'Maybe' is also an instance of 'Functor'. Following our intuition
   that 'fmap' should not change the shape of the data, only the data
   stored within, we map 'Nothing' to 'Nothing', and 'Just' to 'Just',
   using 'f' to transform the data in the latter case: -}

instance Functor Maybe where
  fmap f Nothing  = Nothing
  fmap f (Just a) = Just (f a)

{- It is worth taking a while to look at these definitions (and the one
   for 'map') to see how they are similar, despite the different kinds
   of container. In every case, the constructor that is being matched
   on the left-hand side ('[]', ':', 'Leaf', 'Node', 'Nothing',
   'Just') also appears on the right-hand side. Also, whenever there
   is a substructure (i.e., the rest of the list, the two subtrees),
   we apply 'fmap' to those as well. Together, these two observations
   indicate that our inituition about functors above was correct -- a
   'mapping' function for a container preserves shapes, but transforms
   stored data. -}


-- FIXME: laws:
-- fmap id x == x
-- fmap g (fmap f x) == fmap (g . f) x


{- To see how thinking in terms of preserving shapes but transforming
   stored values -- thinking in Functors -- can help with structuring
   programs, we'll look again at Question 2.20 from Exercise 2. We'll
   need the 'Process' type again: -}

data Process
  = End
  | Output Bool Process
  | Input Process Process
  deriving Show

{- The question asked you to write a new version of the 'process'
   function that (a) returned 'Nothing' when there wasn't enough
   input, and (b) also returned any left over input. The 'process2'
   function has type: -}

process2 :: Process -> [Bool] -> Maybe ([Bool], [Bool])
{- The 'End' case signals that (a) everything is OK; and (b) that all of
   the input it is given is left over: -}
process2 End           bs     = Just ([], bs)
{- If we try to do an 'Input', but there is no input left then we signal
   an error by returning 'Nothing'. -}
process2 (Input tp fp) []     = Nothing
{- If there is some input, then we pick the appropriate choice 'tp' or
   'fp', and then carry on with processing the rest of the input. -}
process2 (Input tp fp) (b:bs) = process2 (if b then tp else fp) bs

{- The case that was trickiest was 'Output'. A recursive use of
   'process2 p bs' yielded a value of type 'Maybe ([Bool],
   [Bool])'. To be able to prepend the 'Bool' being output, we had to
   do a pattern match. If processing the rest of the output gave
   'Nothing', then we give 'Nothing'. If processing the rest of the
   output gave 'Just' something, then we prepend the output: -}
process2 (Output b p)  bs     =
--   case process2 p bs of
--     Nothing       -> Nothing
--     Just (os, ls) -> Just (b:os, ls)
{- Looking at this, and comparing it to the definition of 'fmap' for
   'Maybe' above, we can see that it has the same shape: 'Nothing' is
   mapped to 'Nothing' and 'Just' is mapped to 'Just'. So, instead of
   doing the pattern matching explicitly, we could use 'fmap' to
   transform the value within the 'Maybe' container: -}
      fmap (\ (os, ls) -> (b:os, ls))
           (process2 p bs)

{- This may seem like pointless "code golf". However, it does have the
   advantage that we are no longer specific to 'Maybe' in this
   case. In Lecture 13, we will see other ways of reporting errors
   than just 'Maybe', that will also be 'Functors'. By writing our
   code using high-level functions like 'fmap', we are more able to
   change the data structures we are using without having to change
   large amounts of code. -}

{-     PART II : FOLDABLE

   
-}

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
