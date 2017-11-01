{-# LANGUAGE FlexibleInstances #-}

module Lec12 where

import Prelude hiding (Applicative (..), Monad (..))

{-    LECTURE 12 : APPLICATIVES AND MONADS

   In the previous lecture we wrote five different evaluators:

   1. A 'pure' evaluator for expressions with just addition and
      numbers ('evaluate').

   2. An evaluator for arithmetic expressions that can also throw
      exceptions ('evaluate2').

   3. An evaluator for arithmetic expressions that can also do
      printing ('evaluate3').

   4. An evaluator for arithmetic expressions that have a 'Choice'
      operator, which collects all the possible results
      ('evaluate4opt3').

   5. An evaluator for arithmetic expressions that have a 'Choice'
      operator, which uses an input stream of bits to resolve the
      choices ('evaluate4opt4').

   Looking back at these 'evaluate' functions, we can see that while
   they all do interesting things with the special features of each of
   their languages (throw and handle exceptions; printing; making
   choices), the core features of numbers and addition remained in all
   of them.

   In this lecture, we will see how to simplify the common parts of
   all these evaluators, so we can concentrate on the novel parts of
   each one. We will do this by making use of the fundamental
   abstractions of "Applicative Functors" and "Monads". -}

{-    PART I : EXCEPTIONS

   Here is the datatype representing expressions with numbers,
   addition, and throwing and catching exceptions. It is the same as
   the 'Expr2' we saw in Lecture 11. -}

data Expr2
  = Number2 Int
  | Add2    Expr2 Expr2
  | Throw2
  | Catch2  Expr2 Expr2
  deriving Show

{- The example program we used using Throw2 and Catch2 is: -}

myProblemProgram :: Expr2
myProblemProgram =
  (Number2 23 `Add2` (Number2 34 `Add2` Throw2)) `Catch2` (Number2 0)

{- And here is the evaluator we wrote for this little language: -}

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

{- The evaluator does the right thing in all cases, but is a little
   unsatisfying. In the 'Add2' case, it seems to be a lot of work to
   say "evaluate e1 and e2, and if either of them fail, the whole
   thing fails". Imagine we want (or someone asks us) to add
   multiplication to our little language. We would have to write
   almost exactly the same code again, but with '*' at the end instead
   of '+':

     evaluate2 (Mul2 e1 e2)   = case evaluate2 e1 of
                                  Nothing -> Nothing
                                 Just n1 -> case evaluate2 e2 of
                                              Nothing -> Nothing
                                              Just n2 -> Just (n1*n2)

   Is there a better way?

   One way to proceed is the following line of thought:

     1. We want to evaluate 'e1' and 'e2' to get back 'Int's.

     2. With our two 'Int's, we want to apply '+' to them.

     3. However, if either of the evaluations fails, then the whole
        thing should fail.

   So, somehow, we want to be able to apply functions that expect
   'Int's to values of type 'Maybe Int'. In Lecture 10, we introduced
   'Functor's which nearly allow us to do this. 'Maybe' is a functor,
   so we have a function 'fmap':

      fmap : (a -> b) -> Maybe a -> Maybe b

   We can try this:

      fmap (+) (evaluate2 e1) :: Maybe (Int -> Int)

   because (+) :: Int -> Int -> Int, and 'evaluate e1 :: Maybe
   Int'. But now we are stuck, we have a 'Maybe (Int -> Int)' and
   'evaluate e2 :: Maybe Int', but no way of applying the first to the
   second. 'fmap' won't work because it expects a function, not a
   function wrapped in a 'Maybe'.

   The solution is to 'lift' function application up to 'Maybe's to
   define a function of type:

      Maybe (a -> b) -> Maybe a -> Maybe b

   In Lecture 3, we called this function 'maybeApply', but here we'll
   call it 'apM' for conciseness. This function applies a function to
   its argument if both are 'Just', and returns 'Nothing' otherwise: -}

apM :: Maybe (a -> b) -> Maybe a -> Maybe b
apM Nothing  _        = Nothing
apM (Just f) Nothing  = Nothing
apM (Just f) (Just a) = Just (f a)

{- Let's see how we can use 'apM' to solve our problem. Starting again,
   and ignoring 'fmap' for the time being, we can try first lifting (+)
   to 'Maybe' by wrapping it in a 'Just':

       Just (+) :: Maybe (Int -> Int -> Int)

   then we can use 'apM' to apply this lifted function to 'evaluate2
   e1'. Keep an eye on what happens to the type:

       Just (+) `apM` evaluate2 e1 :: Maybe (Int -> Int)

   Now we have a lifted function again, so we can use 'apM' again:

       Just (+) `apM` evaluate2 e1 `apM` evaluate2 e2 :: Maybe Int

   We've now solved our problem: instead of manually plumbing the
   decision making through the 'Add2' case, we can wrap it all up in
   the 'apM' function. We can rewrite our evaluation function to use
   'apM':

      evaluate2 (Add2 e1 e2) = Just (+) `apM` evaluate2 e1 `apM` evaluate2 a2

   If we what to extend 'Expr2' we multiplication, the case for
   multiplication also becomes much simpler:

      evaluate2 (Mul2 e1 e2) = Just (*) `apM` evaluate2 e1 `apM` evaluate2 a2

     EXERCISE: we started using 'fmap' above, but switch to using
     'apM'. Can you see a way to make the case for 'Add2' simpler
     using 'fmap' as well as 'apM'? -}



{-       PART II : PRINTING

   The syntax of our little language with numbers, addition, and
   printing is defined using the following type. This is exactly the
   same as the 'Expr3' type from Lecture 11: -}

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

{- Here's the evaluation function for this langauge that we defined in Lecture 11. -}

evaluate3 :: Expr3 -> (String, Int)
evaluate3 (Number3 n)  = ("", n)
evaluate3 (Add3 e1 e2) = (s1 ++ s2, n1 + n2)
  where (s1, n1) = evaluate3 e1
        (s2, n2) = evaluate3 e2
{-
-}
evaluate3 (Print3 s e) = (s ++ s1, n)
  where (s1, n) = evaluate3 e

{- As above, the addition case has plumbing that we'd have to repeat if
   we wanted to add new arithmetic operators to the langauge. For
   example, to add multiplication, we'd write:

       evaluate3 (Mul3 e1 e2) = (s1 ++ s2, n1 * n2)
         where (s1, n1) = evaluate3 e1
               (s2, n2) = evaluate3 e2

   where the only difference is replacing '+' with '*'.

   Can we do the same tidying as we did with 'Maybe' above? For
   'Maybe', we defined a special function 'apM' that applied functions
   even when they were wrapped in 'Maybe's. Let's try the same thing
   again. Instead of 'Maybe a', the type in question is now '(String,
   a)', so let's write a function with type: -}

apP :: (String, a -> b) -> (String, a) -> (String, b)
{- Now we are "lifting" function application to values that are paired
   with 'String's. To define such a function, we have to do two things
   -- actually apply the function, and combine the 'String's
   somehow. Appending the 'String's seems like the most sensible thing
   to do, so let's do that: -}
apP (s1, f) (s2, a) = (s1 ++ s2, f a)

{- Looking at our solution above for 'Maybe', we can also see that we
   needed a way to "lift" values up to some kind of "adorned" value
   ('Maybe a' or '(String,a)'). In the case of 'Maybe a', the lifting
   operation was the 'Just' constructor. For printing, it is pairing
   with the empty string: -}

pureP :: a -> (String, a)
pureP a = ("", a)

{- Now we can rewrite the 'Add3' case to use our two new functions, in
   almost exactly the same way as we did for 'Maybe':

      evaluate3 (Add3 e1 e2) = pureP (+) `apP` evaluate3 e1 `apP` evaluate3 e2

   And, as before, if we want to add a 'Mul3' constructor to the
   language, then it is only a little work to update the evaluator to
   handle it:

      evaluate3 (Mul3 e1 e2) = pureP (*) `apP` evaluate3 e1 `apP` evaluate3 e2
-}


{- We've now seen two situations where we defined a kind of "lifted
   function application" to allow use to pretend we are doing normal
   functional programming in the presence of "side effects" (i.e.,
   throwing exceptions or printing). As we did with 'Functor's and
   'Foldable's in Lecture 10, we factor out the common parts of these
   definitions into a general typeclass, which we call 'Applicative': -}

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

{- A 'Functor' f is also 'Applicative' if (in addition to having an
   'fmap' function), it has a function 'pure' that "lifts" values into
   'f', and a function '<*>' that does "lifted" function
   application. Using the definitions of 'apM', 'apP' and 'pureP' we
   gave above, we can give instances for 'Maybe' and '(String,)': -}

instance Applicative Maybe where
  pure = Just
  mf <*> ma = apM mf ma

instance Applicative ((,) String) where
  pure = pureP
  pf <*> pa = apP pf pa

{- Applicative functors are very useful because they allow us to do
   "normal functional programming" while also doing some other things
   on the side, like possibly throwing exceptions, or doing some
   printing. We will see several other examples of Applicative
   functors later in the course. Applicative functors were originally
   introduced by Conor McBride and Ross Paterson in their paper
   "Applicative Programming with Effects":

      Conor McBride and Ross Paterson. Applicative Programming with
      Effects. Journal of Functional Programming 18:1 (2008), pages
      1-13

      http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf
      https://doi.org/10.1017/S0956796807006326
-}

{-   PART III : IF-THEN-ELSE / CONDITIONALS

   Applicative functors are very useful for programming functionally
   in the presence of effects. But are they everything we need? Let's
   consider an extension of 'Expr2', the little language with numbers,
   addition, and exceptions that extends it with an "IfThenElse"
   constructor: -}

data Expr5
  = Number5      Int
  | Add5         Expr5 Expr5
  | Throw5
  | If0ThenElse5 Expr5 Expr5 Expr5
  deriving Show

{- The intended semantics of 'If0ThenElse5 condE thenE elseE' is that
   'condE' is evaluated. If it throws an exception, then the whole
   expression throws. Otherwise, if the result is '0', the expression
   'thenE' is evaluated, and if it is any other number, then 'elseE'
   is evaluated.

   With this in mind, the following expression ought to evaluate to
   '0', because '(-2) + 32 == 30', which is not '0'. -}

conditionProg :: Expr5
conditionProg = If0ThenElse5 (Add5 (Number5 (-2)) (Number5 32))
                             (Number5 1)
                             (Number5 0)

{- The evaluator for this langauge has the same type as for 'Expr2's
   evaluator, because we still have the possibilty of throwing
   exceptions. The cases for numbers, addition and throwing are all
   the same. Here, we use the "official" applicative functor operators
   to keep the code for the number and addition cases tidy. -}

evaluate5 :: Expr5 -> Maybe Int
evaluate5 (Number5 n)  = pure n
evaluate5 (Add5 e1 e2) = pure (+) <*> evaluate5 e1 <*> evaluate5 e2
evaluate5 Throw5       = Nothing
{- Evaluating 'If0ThenElse5 condE thenE elseE' is not as simple. Our first thought might be to write:

     evaluate5 (If0ThenElse5 condE thenE elseE) =
        if evaluate5 condE == 0 then evaluate5 thenE else evaluate5 elseE

   But this doesn't typecheck: 'evaluate5 condE' is of type 'Maybe
   Int', but '0' is of type 'Int', so it doesn't make sense to compare
   them. We could try to fix the type error by wrapping the '0' in a
   'Just':

     evaluate5 (If0ThenElse5 condE thenE elseE) =
        if evaluate5 condE == Just 0 then evaluate5 thenE else evaluate5 elseE

   But this doesn't do the right thing: if 'evaluate5 condE' throws an
   exception (i.e., returns 'Nothing'), then the test will return
   'False' and we will evaluate the else branch. What we should have
   done is signalled an exception by returning 'Nothing'.

   A third attempt would be to use the fact that 'Maybe' is a member
   of the 'Applicative' type class and split out the "pure" and
   "effectful" parts of the computation. If we ignore the possibility
   of exceptions for the moment, and replace the results of evaluating
   'condE', 'thenE', and 'elseE' by 'c', 't', and 'e', respectively,
   then we might think that we can represent the pure part of
   evaluating an 'If0ThenElse5' as:

       if c == 0 then t else e

   Then we can use a lambda to turn this into a function that we
   "lift" using 'pure', and apply to the results of evaluating
   'condE', 'thenE', and 'elseE' using '<*>':

       evaluate5 (If0ThenElse5 condE thenE elseE) =
          pure (\c t e -> if c == 0 then t else e) <*> evaluate5 condE
                                                   <*> evaluate5 thenE
                                                   <*> evaluate5 elseE

   This type checks, and looks like it might plausibly do the right
   thing, but unfortunately it doesn't properly implement the desired
   semantics for 'If0ThenElse5' that we described above. If we use the
   following test case:

      fredTest :: Expr5
      fredTest = If0ThenElse5 (Number5 0)
                              (Number5 17)
                              Throw5

   Looking at the semantics of 'If0ThenElse5' we wrote above, then
   this should evaluate to 'Just 17' -- the exception thrown by the
   "else" branch should never be executed.

   However, if we use the faulty implementation of 'evaluate5', we
   will get:

       > evaluate5 fredTest
       Nothing

    Somehow the exception from the "else" branch has leaked into the
    the "then" branch!

    We can see how this happened by thinking about what '<*>' does. It
    combines all the effects of the things it is applied to. So it has
    gathered the effects from:

       - The 'pure' part -- no effects so OK.
       - The evaluation of 'condE' -- this is OK.
       - The evaluation of 'thenE' -- this is possibly not OK.
       - The evaluation of 'elseE' -- this is possibly not OK.

    The problem is that we don't know which of the effects of 'thenE'
    or 'elseE' we want, until we have looked at the value returned by
    evaluating 'condE'. We can't fix this problem just by using the
    'Applicative' interface. We can see this by looking at the type of
    '<*>':

        (<*>) :: f (a -> b) -> f a -> f b

    In the first argument, we can see that the 'b' value can depend on
    'a', but the 'f' part, which describes the effects, cannot depend
    on the 'a' value. Thinking purely in terms of types, we can fix
    this by moving to a new type:

        (>>=) :: f a -> (a -> f b) -> f b

    We have moved the 'a ->' outside the 'f'. Now the effects (the 'f'
    part) can depend on the value of 'a'. Applicative functors 'f'
    that also have an implementation of this function are called
    "Monads". The symbol (>>=) is usually pronounced "bind", the
    reason for this will become clearer when we look at "do notation"
    in the next lecture.

    Once we have a function of this type, we can write a correct
    evaluator for 'If0ThenElse5', which correctly delays the
    evaluation of 'thenE' and 'elseE' until we know which one we want
    to execute: -}

evaluate5 (If0ThenElse5 condE thenE elseE) =
  evaluate5 condE >>= \ n ->
  if n == 0 then evaluate5 thenE else evaluate5 elseE

{- The syntax for using (>>=) may appear confusing at first sight. Try
   writing parentheses around the first argument to '>>=' and the
   lambda expression to make it clearer which parts are the
   parameters, like so:

       evaluate5 (If0ThenElse5 condE thenE elseE) =
         (evaluate5 condE) >>= (\ n ->
         if n == 0 then evaluate5 thenE else evaluate5 elseE)

   As we mentioned above, an Applicative Functor that also has a
   function '>>=' ("bind") with the type given above is called a
   "Monad". We define another typeclass to capture the idea of Monads: -}

class Applicative m => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

{- For historical reasons, the Monad type class has an additional
   member, called 'return', which must always be the same as 'pure' in
   the Applicative instance for the same type.

   The monad instance for 'Maybe' is defined like so: -}

instance Monad Maybe where
  return = pure
  
  Nothing  >>= f = Nothing
  (Just a) >>= f = f a

{- 'return' must always be the same as 'pure', so we define it like that.

   (>>=) is defined using the following thinking, if we have a 'Maybe'
   computation 'c' and a function 'f' that is expecting a value from
   'c', then what we do depends on 'c'. If 'c' is 'Nothing', then we
   cannot use 'f' (because it is expecting an 'a' that we don't have),
   so we must return 'Nothing'. If 'c' is 'Just a', then we use 'f a
   :: Maybe b' as the result. Compare this definition with the
   'Functor' instance for 'Maybe' in Lecture 10.

     EXERCISE: Define a Monad instance for the printing type '(,)
     String'. We will come back to this in the next lecture.

   One useful function for 'Monad's is like the '>>=' function, but
   ignores the first arguments result. This is something like the
   'semicolon' statement separator in imperative programming
   languages. We will see examples of this in the next few lectures. -}

(>>) :: Monad m => m a -> m b -> m b
ma >> mb = ma >>= (\ _ -> mb)

