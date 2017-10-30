module Lec13 where

{-    LECTURE 13 : MONADS WE LIKE -}

import Prelude hiding (print, mapM, filterM, Either(..))
--import Prelude hiding (print, mapM, filterM)

{-
class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class Applicative f => Monad f where
  (>>=) :: f a -> (a -> f b) -> f b
-}

----------------------------------------------------------------------
-- do notation







-- Maybe

{-
instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just a) = Just (f a)

instance Applicative Maybe where
  pure = Just
  Just f <*> Just a = Just (f a)
  _ <*> _           = Nothing
-}

abort :: Maybe a
abort = Nothing



----------------------------------------------------------------------
-- Either

data Either a b = Left a | Right b
  deriving (Eq, Show)












lookupWithError :: (Eq a, Show a) => a -> [(a, b)] -> Either String b
lookupWithError = undefined

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


----------------------------------------------------------------------
-- Lists





triples :: [Int]
triples = undefined

triples' :: [Int]
triples' = undefined

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

-- CP a b from Ex3

-- forward ref to IO

-- forward ref to Parser

-- forward ref to State


----------------------------------------------------------------------
-- Functors vs applicatives vs monads

{-
functor     - fmap  :: (a -> b)   -> m a -> m b
applicative - (<*>) :: m (a -> b) -> m a -> m b
monad       - bind  :: (a -> m b) -> m a -> m b
-}

----------------------------------------------------------------------
-- Generic functions

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM = undefined

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA = undefined

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM = undefined
