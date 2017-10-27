module Lec12 where

{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (Applicative (..), Monad (..))

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


evaluate2 :: Expr2 -> Maybe Int
evaluate2 (Number2 n)    = Just n
evaluate2 (Add2 e1 e2)   = case evaluate2 e1 of
                             Nothing -> Nothing
                             Just n1 -> case evaluate2 e2 of
                                          Nothing -> Nothing
                                          Just n2 -> Just (n1+n2)
evaluate2 Throw2         = Nothing
evaluate2 (Catch2 e1 e2) = case evaluate2 e1 of
                             Nothing -> evaluate2 e2
                             Just n  -> Just n

----------------------------------------------------------------------



----------------------------------------------------------------------
-- Printing

data Expr3
  = Number3 Int
  | Add3    Expr3 Expr3
  | Print3  String Expr3
  deriving Show

printingProg :: Expr3
printingProg =
  (Print3 "Hello" (Number3 23))
  `Add3`
  (Number3 34 `Add3` (Print3 " World" (Number3 56)))

evaluate3 :: Expr3 -> (String, Int)
evaluate3 (Number3 n)  = ("", n)
evaluate3 (Add3 e1 e2) = (s1 ++ s2, n1 + n2)
  where (s1, n1) = evaluate3 e1
        (s2, n2) = evaluate3 e2
evaluate3 (Print3 s e) = (s ++ s1, n)
  where (s1, n) = evaluate3 e

----------------------------------------------------------------------


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

evaluate5 :: Expr5 -> Maybe Int
evaluate5 = undefined

----------------------------------------------------------------------


----------------------------------------------------------------------


----------------------------------------------------------------------
