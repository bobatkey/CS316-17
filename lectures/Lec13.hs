module Lec13 where

{-    LECTURE 13 : MONADS WE LIKE -}

import Prelude hiding (mapM, filterM)

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
-- do notation (Bob)

-- Maybe (Bob)

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

-- FIXME: an example program

----------------------------------------------------------------------
-- Either (Fred)



----------------------------------------------------------------------
-- Lists (Bob)

triples = do x <- [1..10]
             y <- [1..10]
             z <- [1..10]
             if x*x + y*y == z*z then pure (x,y,z) else []

triples' = [ (x,y,z) | x <- [1..10], y <- [1..10], z <- [1..10], x*x + y*y == z*z ]

----------------------------------------------------------------------
-- Printing (Fred)

data Printing a = MkPr String a

print :: String -> Printing ()
print s = MkPr s ()

----------------------------------------------------------------------
-- Reader (Bob)

data Reader r a = MkR (r -> a)

-- CP a b from Ex3

-- forward ref to IO

-- forward ref to Parser

-- forward ref to State

----------------------------------------------------------------------
-- Fred
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f []     = pure []
mapM f (x:xs) = do b  <- f x
                   bs <- mapM f xs
                   pure (b : bs)

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA = undefined

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM = undefined

