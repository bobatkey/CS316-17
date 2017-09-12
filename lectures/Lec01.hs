module Lec01 where

data List a
  = Nil
  | Cons a (List a)

ex1 :: List Int
ex1 = Cons 12 (Cons 17 Nil)

-- ex2 :: List Int
-- ex2 = Cons (Cons 1 Nil) (Cons 2 Nil)

total :: List Int -> Int
total Nil         = 0                          -- 1
total (Cons x xs) = x + total xs               -- 2

append :: List a -> List a -> List a
append Nil         ys = ys                     -- 3
append (Cons x xs) ys = Cons x (append xs ys)  -- 4

