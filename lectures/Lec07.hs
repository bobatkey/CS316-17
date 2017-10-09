module Lec07 where

import Prelude hiding
  (Maybe (..), String)

{-    LECTURE 07 : DECLARING TYPES AND CLASSES

-}

{-    PART I : TYPE SYNONYMS -}

type String = [Char]

message :: String
message = "Help I'm stuck in a Haskell program!"

type Position = (Float, Float)

origin :: Position
origin = (0.0, 0.0)

type Transformation = Position -> Position

doNothing :: Transformation
doNothing x = x

mirror :: Transformation
mirror (x,y) = (-x, y)

type Pair a = (a,a)

type Position2 = Pair Float

-- type List = (Integer, List)


{-    PART II : DATA TYPES -}

data List a
  = Nil
  | Cons a (List a)

data Direction
  = North
  | South
  | East
  | West
  deriving Show

move :: Direction -> Transformation
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East  (x,y) = (x+1,y)
move West  (x,y) = (x-1,y)


data Maybe a = Nothing | Just a

safeDivision :: Int -> Int -> Maybe Int
safeDivision x 0 = Nothing
safeDivision x y = Just (x `div` y)

-- Recursive type

data Natural
  = Zero
  | Succ Natural

zero = Zero
one = Succ zero
two = Succ one
three = Succ two

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)

data LeafyTree a
  = Leafy a
  | Nodey (LeafyTree a) (LeafyTree a)

data RoseTree a = Rose a [RoseTree a]

data XML
  = Element String [XML]     -- <blah> .... </blah>
  | Text String

data JSON
  = Null
  | Bool   Bool
  | String String
  | Number Float
  | Array  [JSON]
  | Object [(String,JSON)]
  deriving (Eq)

hd :: [a] -> Maybe a
hd [] = Nothing
hd (x:xs) = Just x

data NEList a
  = NECons a (Maybe (NEList a))

hd' :: NEList a -> a
hd' (NECons x xs) = x

neListToList :: NEList a -> [a]
neListToList (NECons x Nothing) = [x]
neListToList (NECons x (Just t)) = x : neListToList t

tl :: NEList a -> [a]
tl (NECons x Nothing) = []
tl (NECons x (Just l)) = neListToList l


{-    PART III : TYPE CLASSES -}


instance Show JSON where
  show Null         = "null"
  show (Bool True)  = "true"
  show (Bool False) = "false"
  show (String s)   = show s
  show (Number x)   = show x
  show (Array xs)   = show xs
  show (Object ps)  = "{" ++ showAnObject ps ++ "}"

showAnObject [] = ""
showAnObject [(name, json)] =
  show name ++ ": " ++ show json
showAnObject ((name, json):ps) =
  show name ++ ": " ++ show json ++ ",\n " ++ showAnObject ps


showTwice :: Show a => a -> String
showTwice x = show x ++ show x

instance Show a => Show (List a) where
  show Nil = "Nil"
  show (Cons x xs) = "(Cons " ++ show x ++ " " ++ show xs ++ ")"

-- instance Show (List a) => Show (List a) where
--   show x = "ksudhkhgdfuhgrhgldij"

-- instance Show (

naturalToInteger :: Natural -> Integer
naturalToInteger Zero = 0
naturalToInteger (Succ n) = 1 + naturalToInteger n

instance Show Natural where
  show n = show (naturalToInteger n)


class Monoid m where
  munit :: m
  mappend :: m -> m -> m

instance Monoid Integer where
  munit = 0
  mappend m n = m + n


