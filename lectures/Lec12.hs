{-# LANGUAGE FlexibleInstances #-}
module Lec12 where

import Prelude hiding (Applicative (..), Monad (..))

{-    LECTURE 12 : APPLICATIVES AND MONADS -}

----------------------------------------------------------------------
-- Exceptions

data Expr2
  = Number2 Int
  | Add2    Expr2 Expr2
  | Throw2
  | Catch2  Expr2 Expr2
  deriving Show

{- An example program using Throw2 and Catch2 is this one: -}

myProblemProgram :: Expr2
myProblemProgram =
  (Number2 23 `Add2` (Number2 34 `Add2` Throw2)) `Catch2` (Number2 0)


apM :: Maybe (a -> b) -> Maybe a -> Maybe b
apM Nothing _ = Nothing
apM (Just f) Nothing = Nothing
apM (Just f) (Just a) = Just (f a)

evaluate2 :: Expr2 -> Maybe Int
evaluate2 (Number2 n)    = Just n
evaluate2 (Add2 e1 e2)   = (Just (+) `apM` (evaluate2 e1)) `apM` (evaluate2 e2)

-- Just (+) :: Maybe (Int -> (Int -> Int))
-- evaluate2 e1 :: Maybe Int
-- Just (+) `apM` (evaluate2 e1) :: Maybe (Int -> Int)
-- evaluate2 e2 :: Maybe Int
  {-

evaluate2 (Mul2 e1 e2)   = case evaluate2 e1 of
                             Nothing -> Nothing
                             Just n1 -> case evaluate2 e2 of
                                          Nothing -> Nothing
                                          Just n2 -> Just (n1*n2)
-}
evaluate2 Throw2         = Nothing
evaluate2 (Catch2 e1 e2) = case evaluate2 e1 of
                             Nothing -> evaluate2 e2
                             Just n  -> Just n

----------------------------------------------------------------------
-- Printing

data Expr3
  = Number3 Int
  | Add3    Expr3 Expr3
  | Mul3    Expr3 Expr3  
  | Op3     (Int -> Int -> Int) Expr3 Expr3
  | Print3  String Expr3
--  deriving Show

printingProg :: Expr3
printingProg =
  (Print3 "Hello" (Number3 23))
  `Add3`
  (Number3 34 `Add3` (Print3 " World" (Number3 56)))

pureP :: a -> (String, a)
pureP a = ("", a)

apP :: (String, a -> b) -> (String, a) -> (String, b)
apP (s1, f) (s2, a) = (s1 ++ s2, f a)

evaluate3 :: Expr3 -> (String, Int)
evaluate3 (Number3 n)  = ("", n)
evaluate3 (Op3 f e1 e2) = (pureP f `apP` evaluate3 e1) `apP` evaluate3 e2
evaluate3 (Add3 e1 e2) = (pureP (+) `apP` evaluate3 e1) `apP` evaluate3 e2
evaluate3 (Mul3 e1 e2) = (pureP (*) `apP` evaluate3 e1) `apP` evaluate3 e2

 
{-
evaluate3 (Add3 e1 e2) = (s1 ++ s2, n1 + n2)
  where (s1, n1) = evaluate3 e1
        (s2, n2) = evaluate3 e2
-}
{-
evaluate3 (Mul3 e1 e2) = (s1 ++ s2, n1 * n2)
  where (s1, n1) = evaluate3 e1
        (s2, n2) = evaluate3 e2
-}
evaluate3 (Print3 s e) = (s ++ s1, n)
  where (s1, n) = evaluate3 e


class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
  pure = Just
  mf <*> ma = apM mf ma

instance Applicative ((,) String) where
  pure = pureP
  pf <*> pa = apP pf pa

----------------------------------------------------------------------
-- Conditions
data Expr5
  = Number5      Int
  | Add5         Expr5 Expr5
  | If0ThenElse5 Expr5 Expr5 Expr5
  | Throw5
  deriving Show

conditionProg :: Expr5
conditionProg = If0ThenElse5 (Add5 (Number5 (-2)) (Number5 32))
                             (Number5 1)
                             (Number5 0)

fredTest :: Expr5
fredTest = If0ThenElse5 (Number5 0)
                        (Number5 17)
                        Throw5

evaluate5 :: Expr5 -> Maybe Int
evaluate5 (Number5 n)  = pure n
evaluate5 (Add5 e1 e2) = pure (+) <*> evaluate5 e1 <*> evaluate5 e2
evaluate5 (If0ThenElse5 condE thenE elseE) =
  evaluate5 condE >>= \ n -> if n == 0 then evaluate5 thenE else evaluate5 elseE
  {- ALTERNATIVE DEFINITION (Small print: wrong)
   pure (\c t e -> if c == 0 then t else e) <*> evaluate5 condE
                                           <*> evaluate5 thenE
                                           <*> evaluate5 elseE
-}
--   if evaluate5 condE == 0 then evaluate5 thenE else evaluate5 elseE
evaluate5 Throw5       = Nothing

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

(>>) :: m a -> m b -> m b
ma >> mb = ma >>= (\ _ -> mb)

instance Monad Maybe where
  Nothing  >>= f = Nothing
  (Just a) >>= f = f a


----------------------------------------------------------------------



















----------------------------------------------------------------------
















----------------------------------------------------------------------
