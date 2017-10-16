module Lec09 where

{- LECTURE 9: RECURSION SCHEMES -}

{-
In Lecture 5, we covered how to define recursive functions --
functions that are defined in terms of themselves.

We can see that they often fall into a few standard "patterns".
Today, we will look at systematically replacing the constructors of a
recursive data type with values and functions.

This pattern is called 'iteration' over the data type.
-}

















data Nat
  = Zero
  | Succ Nat
  deriving Show





















plus :: Nat -> Nat -> Nat
plus m n = undefined
























{-
We see that 'n' is unchanged throughout; can rewrite to make this
clearer:
-}

plus' :: Nat -> Nat -> Nat
plus' m n = undefined























{-
Two observations:

1. There is a line for each constructor of the 'Nat' type.

2. The recursive call in the 'Succ' case is on 'm';
   We don't look at 'm' directly, only at the value recursively
   generated from it.
-}

iterNat :: a -> (a -> a) -> Nat -> a
iterNat zero succ n = undefined

{- The type of 'iterNat' states:

     iterNat :: a        -- a value to use for 'Zero'
             -> (a -> a) -- a function to use for 'Succ'
             -> Nat      -- a Nat to look at
             -> a        -- the value returned by looking at the Nat
-}























plus2 :: Nat -> Nat -> Nat
plus2 m n = undefined

























{-
Another way to write 'plus' using 'iterNat' is to pass around 'n' each
time, just as we did in the first definition.
-}

plus3 :: Nat -> (Nat -> Nat)
plus3 m = undefined




























{-
What happens when we use 'Zero' as the value for 'Zero', and 'Succ' as
the function for 'Succ'?
-}

thingy :: Nat -> Nat
thingy = iterNat Zero Succ























{-
Let's look at another example of a function on 'Nat's: the equality
testing function.
-}

eqNat0 :: Nat -> Nat -> Bool
eqNat0 = undefined





























{- Explicitly pattern matching seems to go against the spirit of
   'iterNat'. Can we replace the 'case' expressions with uses of
   'iterNat'?
-}

eqNat1 :: Nat -> Nat -> Bool
eqNat1 = iterNat zeroCase succCase -- 'a = Nat -> Bool'
 where zeroCase :: Nat -> Bool
       zeroCase = undefined

       succCase :: (Nat -> Bool) -> Nat -> Bool
       succCase eqNat_m = undefined































caseNat :: a -> (Nat -> a) -> Nat -> a
caseNat zero succ n = undefined

{-
'caseNat' is similar to 'iterNat', except that it does not call
itself recursively. The 'Nat' 'k' is passed directly into the
'succ' function.
-}
































eqNat :: Nat -> Nat -> Bool
eqNat = undefined




























{-
Is there a recursion scheme that gives us access to both the
recursive result and the value being examined?
-}

recNat :: a -> ((Nat,a) -> a) -> Nat -> a
recNat zero succ n = undefined

{-
Compared to 'caseNat', 'recNat' calls itself recursively. Compared to
'iterNat', the 'Succ n' case passes 'n' to the 'succ' function.
-}






























recNatFromIterNat :: b -> ((Nat,b) -> b) -> Nat -> b
recNatFromIterNat zero succ n = undefined


































data Expr
  = Number Int
  | Add    Expr Expr
  deriving Show


