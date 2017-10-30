module Lec13 where

import Prelude hiding (print, mapM, filterM, Either(..))

{-     LECTURE 13 : MONADS WE LIKE

   In the previous lecture, we introduced monads as a way to tidy up
   evaluators for languages that had different kinds of side effects
   (exceptions, printing, non-deterministic choice). In this lecture,
   we look at Monads by themselves, as a way of structuring all sorts
   of programs.

   We'll look at:

     1. Several different examples of Monads: 'Maybe', Either, Lists,
        Writer.

     2. The idea of 'operations' associated with a monad.

     3. "do notation", a way of making some programs written using
        monads more readable.

     4. Some programs that can be written for all monads. -}

{-
   Recap:

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class Applicative f => Monad f where
  (>>=) :: f a -> (a -> f b) -> f b

-}

----------------------------------------------------------------------

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

----------------------------------------------------------------------
-- Either

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

lookupEither :: (Show a, Eq a) => a -> [(a,b)] -> Either String b
lookupEither key table = case lookupMaybe key table of
                           Nothing -> Left ("key not in table: " ++ (show key))
                           Just v -> Right v

nutritiousEither :: String -> String -> Either String String
nutritiousEither fruit1 fruit2 = do
  cal1 <- lookupEither fruit1 fruitNutrition
  cal2 <- lookupEither fruit2 fruitNutrition
  let highest = if cal1 > cal2 then fruit1 else fruit2
  pure (highest ++ " is the most nutritious fruit")  



----------------------------------------------------------------------
-- Lists

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
  (<*>) :: [a -> b] -> [a] -> [b]
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

----------------------------------------------------------------------
-- Printing/trace

data Printing a = MkPr String a

instance Show a => Show (Printing a) where
  show (MkPr msg a) = msg ++ "\n=====\n\n" ++ (show a)


fib :: Integer -> Printing Integer
fib n = undefined

----------------------------------------------------------------------
-- Reader

data Reader r a = MkR (r -> a)

data Environment = Opt { username    :: String
                       , isSuperuser :: Bool
                       }
  deriving Show

ask :: Reader a a
ask = MkR (\x -> x)


-- CP a b from Ex3

-- sequ :: CP c a -> (a -> CP c b) -> CP c b

-- forward ref to Parser

-- forward ref to State

-- forward ref to IO



----------------------------------------------------------------------
-- Functors vs applicatives vs monads

{-
functor     - fmap  ::   (a ->   b) -> m a -> m b
applicative - (<*>) :: m (a ->   b) -> m a -> m b
monad       - bind  ::   (a -> m b) -> m a -> m b
-}













----------------------------------------------------------------------
-- Generic functions

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM = undefined

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA = undefined

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM = undefined
