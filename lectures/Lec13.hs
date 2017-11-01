module Lec13 where

import Prelude hiding (print, mapM, filterM, Either(..))

{-     LECTURE 13 : MONADS WE LIKE

   In the previous lecture, we introduced monads as a way to tidy up
   evaluators for languages that had different kinds of side effects
   (exceptions, printing, non-deterministic choice). In this lecture,
   we start to look at Monads by themselves, as a way of structuring
   all sorts of programs.

   We will concentrate on the use of Monads for doing search-like
   operations, like looking things up in a database. The database we
   will use for our examples is the following table of fruits with
   their calorie content: -}

fruitNutrition :: [(String, Int)]
fruitNutrition =
  [ ("apple", 130)
  , ("avocado", 250)
  , ("banana", 110)
  , ("grapefruit", 120)
  , ("lemon", 15)
  , ("orange", 80)
  , ("pear", 60)
  , ("watermelon", 1440)
  ]

{- In this lecture, we'll also introduce "do notation", which is a way
   of making some programs written using monads more readable.

   Recap:

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class Applicative f => Monad f where
  (>>=) :: f a -> (a -> f b) -> f b

-}

{-   PART I : THE MAYBE MONAD -- EXCEPTIONS / FAILURE -}


{-
instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just a) = Just (f a)

instance Applicative Maybe where
  pure = Just
  Just f <*> Just a = Just (f a)
  _ <*> _           = Nothing

instance Monad Maybe where
  Nothing >>= f = Nothing
  Just x  >>= f = f x
-}

abort :: Maybe a
abort = Nothing

lookupMaybe :: Eq a => a -> [(a,b)] -> Maybe b
lookupMaybe key [] = abort
lookupMaybe key ((key',v):kvs)
  | key == key' = pure v
  | otherwise   = lookupMaybe key kvs

nutritious :: String -> String -> Maybe String
nutritious fruit1 fruit2 = do
  cal1 <- lookupMaybe fruit1 fruitNutrition
  cal2 <- lookupMaybe fruit2 fruitNutrition
  let highest = if cal1 > cal2 then fruit1 else fruit2
  pure (highest ++ " is the most nutritious fruit")

-- do x <- e1
--    e2
--
--    e1 >>= \x -> e2
--
--
-- do e1
--    e2
--
--   --->
--
--    e1 >> e2

-- nutritious fruit1 fruit2 =
--   lookupMaybe fruit1 fruitNutrition >>= \cal1 ->
--   lookupMaybe fruit2 fruitNutrition >>= \cal2 ->
--   let highest = if cal1 > cal2 then fruit1 else fruit2 in
--   pure (highest ++ " is the most nutritious fruit")


{-    PART 2 : THE EITHER MONAD -- EXCEPTIONS WITH REASONS -}

data Either a b = Left a | Right b
  deriving (Eq, Show)

ex1 :: Either String Int
ex1 = Left "hi"

ex2 :: Either String Int
ex2 = Right 17

instance Functor (Either c) where
--  fmap :: (a -> b) -> Either c a -> Either c b
   fmap f (Left c) = Left c
   fmap f (Right a) = Right (f a)

instance Applicative (Either c) where
--  pure :: a -> Either c a
   pure = Right 
  --  (<*>) :: Either c (a -> b) -> Either c a -> Either c b
   (Left c) <*> ea = Left c
   (Right f) <*> (Left c) = Left c
   (Right f) <*> (Right a) = Right (f a)

instance Monad (Either c) where
  -- (>>=) :: Either c a -> (a -> Either c b) -> Either c b
  (Left c)  >>= f = (Left c)
  (Right a) >>= f = f a

abortWithMessage :: String -> Either String a
abortWithMessage s = Left s

lookupEither :: (Show a, Eq a) => a -> [(a,b)] -> Either String b
lookupEither key table =
  case lookupMaybe key table of
    Nothing ->
      abortWithMessage ("key not in table: " ++ (show key))
    Just v ->
      pure v

nutritiousEither :: String -> String -> Either String String
nutritiousEither fruit1 fruit2 = do
  cal1 <- lookupEither fruit1 fruitNutrition
  cal2 <- lookupEither fruit2 fruitNutrition
  let highest = if cal1 > cal2 then fruit1 else fruit2
  pure (highest ++ " is the most nutritious fruit")  

{-
     > nutritiousEither "watermelon" "bapple"
     Left "key not in table: bapple"

-}


{-    PART 3 : LISTS, or DOING SEARCH WITH MONADS -}

-- FIXME: change this to use the database, and refer to LINQ and
-- 'flatMap'.

triples :: [(Integer,Integer,Integer)]
triples = do
  x <- [1..10]
  y <- [1..10]
  z <- [1..10]
  if x*x + y*y == z*z then pure (x,y,z)
  else []

{-
-- fmap :: (a -> b) -> [a] -> [b]

instance Applicative [] where
  -- (<*>) :: [a -> b] -> [a] -> [b]
  []     <*> xs = []
  (f:fs) <*> xs = fmap f xs ++ (fs <*> xs)

instance Monad [] where
  -- (>>=) :: [a] -> (a -> [b]) -> [b]
  []     >>= f = []
  (x:xs) >>= f = f x ++ xs >>= f
-}

triples' :: [(Integer, Integer, Integer)]
triples' = [ (x,y,z) | x <- [1..10]
                     , y <- [1..10]
                     , z <- [1..10]
                     , x*x + y*y == z*z ]



{-    PART 4 : FUNCTORS vs APPLICATIVES vs MONADS -}

{-
functor     - fmap  ::   (a ->   b) -> m a -> m b
applicative - (<*>) :: m (a ->   b) -> m a -> m b
monad       - bind  ::   (a -> m b) -> m a -> m b
-}


{-    PART 5 : Other examples of Monads -}

-- CP a b from Ex3

-- sequ :: CP c a -> (a -> CP c b) -> CP c b

-- forward ref to Parser

-- forward ref to Reader, Writer, and State

-- forward ref to IO



