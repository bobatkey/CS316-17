{-# LANGUAGE RebindableSyntax #-}

module Lec13 where

import Prelude hiding (Either(..), Functor (..), Applicative (..), Monad (..))

-- We need the following definitions in order to use
-- 'RebindableSyntax' for our own declaration of the 'Monad'
-- typeclass. Normal programs don't need these.

ifThenElse True t e = t
ifThenElse False t e = e

(>>) :: Monad m => m a -> m b -> m b
ma >> mb = ma >>= \_ -> mb

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
  [ ("apple",       130)
  , ("avocado",     250)
  , ("banana",      110)
  , ("grapefruit",  120)
  , ("lemon",        15)
  , ("orange",       80)
  , ("pear",         60)
  , ("watermelon", 1440)
  ]

{- In this lecture, we'll also introduce "do notation", which is a way
   of making some programs written using monads more readable.

   Let's first recap the three different typeclasses we've defined so
   far to capture some notion of "side effects" that happen while we
   are computing a value.

   The first type class was 'Functor'. 'Functor's capture a kind of
   "container-ness": if we have a container full of 'a's and a way of
   transforming 'a's to 'b's, then we can get a container full of 'b's
   with the "same shape". When we think in terms of pure values with
   attached "side effects", we think of the "shape" as the side effect
   that happened while producing the value. The 'Functor' typeclass is
   defined like as: -}

class Functor f where
  fmap :: (a -> b) -> f a -> f b

{- The second type class was 'Applicative'. This captures the idea that
   if we have a way of getting functions, possibly with some side
   effects, and arguments for those functions, again possibly with
   side effects, then there is a way of combining the sideeffects to
   apply the function to the argument. We also have a 'pure' function
   that allows us to "lift" normal values to ones with "side effects",
   where the "side effect" is somehow "do nothing". See Lecture 12 for
   more details. -}

class Functor f => Applicative f where
  pure :: a -> f a

  (<*>) :: f (a -> b) -> f a -> f b

{- The third type class was 'Monad'. This extends the idea of
   'Applicative', but also allows the function part to do different
   side effects based on the value of it sargument. As we saw in
   Lecture 12, this is essential for writing programs that make
   decisions. -}

class Applicative f => Monad f where
  return :: a -> f a
  (>>=) :: f a -> (a -> f b) -> f b

  fail :: String -> f a
  fail s = error "fail"

{- For historical reasons, the 'Monad' typeclass also includes two extra
   members that are not necessarily strictly required: 'return' should
   always be the same as 'pure', and 'fail' is a design mistake in the
   original Haskell definition that should never have
   happened. However, we must include it here for the "do notation"
   that we use later on to work. -}

{-   PART I : THE MAYBE MONAD -- EXCEPTIONS / FAILURE

   In Lectures 10 and 12 we gave the 'Functor', 'Applicative', and
   'Monad' instances for 'Maybe', and here they are again: -}

instance Functor Maybe where
  fmap f Nothing  = Nothing
  fmap f (Just a) = Just (f a)

instance Applicative Maybe where
  pure = Just
  Just f <*> Just a = Just (f a)
  _ <*> _           = Nothing

instance Monad Maybe where
  return = pure

  Nothing >>= f = Nothing
  Just x  >>= f = f x

{- In Lectures 11 and 12, we wrote 'Nothing' to indicate failure / the
   throwing of an exception. From now on, we'll try to be a bit more
   highlevel, and give more descriptive names to the operations that
   each individual 'Monad' supports. We call the operation of
   "failing" 'abort': -}

abort :: Maybe a
abort = Nothing

{- We can now write a function that uses 'pure' and 'abort' to indicate
   success or failure when searching through a database of (key,
   value) pairs: -}

lookupMaybe :: Eq a => a -> [(a,b)] -> Maybe b
lookupMaybe key [] = abort
lookupMaybe key ((key',v):kvs)
  | key == key' = pure v
  | otherwise   = lookupMaybe key kvs

{- If 'lookupMaybe key db' doesn't find 'key' in 'db', then it signals
   this by using 'abort'. We can now use 'lookupMaybe' to search the
   database twice to find the nutritional value of two fruits and
   compare them.

   Since 'lookupMaybe' on the 'fruitNutrition' database returns a
   'Maybe Int', we can't use the value directly. Instead, we must use
   the '>>=' operator to make sure that errors are properly
   handled. This is similar to the way we used '>>=' at the end of
   Lecture 12. -}

nutritious :: String -> String -> Maybe String
nutritious fruit1 fruit2 =
  lookupMaybe fruit1 fruitNutrition >>= \cal1 ->
  lookupMaybe fruit2 fruitNutrition >>= \cal2 ->
  let highest = if cal1 > cal2 then fruit1 else fruit2 in
  pure (highest ++ " is the most nutritious fruit")

{- Trying out 'nutritious', we can see it reports success by returning
   'Just <something>' and failure by returning 'Nothing':

       λ> nutritious "apple" "watermelon"
       Just "watermelon is the most nutritious fruit"

       λ> nutritious "bapple" "watermelon"
       Nothing

      EXERCISE: Try writing outnutritious using explicit 'case's instead
      of '>>=' (as we did in the first version of 'evaluate2' in
      Lecture 11). This should help you see how much more concise we
      can be using the 'Monad' functions.

   Even though the '>>=' saves quite a bit of typing, it is still a
   bit "line noise"-like. Haskell provides a nice "do notation" for
   writing programs that use '>>=' a lot. Instead of writing:

       e1 >>= \x -> e2

   we can write

       do e1
          e2

   Let's rewrite 'nutritious' to use this: -}

nutritious' :: String -> String -> Maybe String
nutritious' fruit1 fruit2 = do
  cal1 <- lookupMaybe fruit1 fruitNutrition
  cal2 <- lookupMaybe fruit2 fruitNutrition
  let highest = if cal1 > cal2 then fruit1 else fruit2
  pure (highest ++ " is the most nutritious fruit")

{- In general, "do notation" is translated away in a process called
   "desugaring" by the following rules:

        do x <- e1
           e2

   becomes

        e1 >>= \x -> e2

   and

        do e1
           e2

   becomes

        e1 >> e2

   (where '>>' is the "do this and then do that" function we defined
   at the end of Lecture 12.)

   This last definition of 'nutritious' is now a bit more readable
   than a cascade of 'case' expressions. However, while it does report
   lookup errors, it doesn't tell us what the error is. We can do this
   by changing the 'Monad' we are using to the 'Either' monad. -}


{-    PART 2 : THE EITHER MONAD -- EXCEPTIONS WITH REASONS -}

data Either a b = Left a | Right b
  deriving (Eq, Show)

ex1 :: Either String Int
ex1 = Left "hi"

ex2 :: Either String Int
ex2 = Right 17

{- The 'Functor' instance for 'Either c' is very similar to the one for
   'Maybe'. The 'Right' case is almost identical to the 'Just'
   case. The 'Left' case is similar to the 'Nothing' case, except that
   we have to also remember to carry over the 'c'.

   We've written the type of 'fmap' specialised to 'Either c' in a
   comment as a guide. -}

instance Functor (Either c) where
-- fmap :: (a -> b) -> Either c a -> Either c b
   fmap f (Left c)  = Left c
   fmap f (Right a) = Right (f a)

{- The 'Applicative' instance for 'Either' is also similar to the one
   for 'Maybe', except that we have to have two separate "failure"
   cases. If the first argument fails with a message 'c', then we
   return that. If the first argument succeeds, but the second fails,
   then we return the second one. Notice that we had a choice of
   ordering here, and we chose to go "left-to-right". -}

instance Applicative (Either c) where
-- pure :: a -> Either c a
   pure = Right 

-- (<*>) :: Either c (a -> b) -> Either c a -> Either c b
   (Left c)  <*> ea        = Left c
   (Right f) <*> (Left c)  = Left c
   (Right f) <*> (Right a) = Right (f a)

{- The 'Monad' instance is also similar to the 'Maybe' one, except that
   we need to pass through the "reason" 'c' in the 'Left' case: -}

instance Monad (Either c) where
  return = pure

  -- (>>=) :: Either c a -> (a -> Either c b) -> Either c b
  (Left c)  >>= f = Left c
  (Right a) >>= f = f a

{- Just as 'Maybe' had an 'abort' operation, 'Either c' has an
   'abortWithMessage' operation, that takes a 'c' as a parameter to
   stand for the message. -}

abortWithMessage :: c -> Either c a
abortWithMessage s = Left s

{- We can now write a 'lookupEither' function that calls 'lookupMaybe'
   to do the actual lookup, but provides a more useful error message
   when the lookup fails. -}

lookupEither :: (Show a, Eq a) => a -> [(a,b)] -> Either String b
lookupEither key table =
  case lookupMaybe key table of
    Nothing ->
      abortWithMessage ("key not in table: " ++ (show key))
    Just v ->
      pure v

{- Now we can write 'nutritious' again, but using our 'lookupEither'
   function. Notice how the "do notation" works for 'Either c' just as
   it did for 'Maybe'. -}

nutritiousEither :: String -> String -> Either String String
nutritiousEither fruit1 fruit2 = do
  cal1 <- lookupEither fruit1 fruitNutrition
  cal2 <- lookupEither fruit2 fruitNutrition
  let highest = if cal1 > cal2 then fruit1 else fruit2
  pure (highest ++ " is the most nutritious fruit")  

{- Now when we make a query that fails, we get a reason:

      > nutritiousEither "watermelon" "bapple"
      Left "key not in table: bapple"

   We can also learn the order in which 'nutritiousEither' does its
   lookups by providing two fruit names that are not in the database,
   and observing what error we get:

      > nutritiousEither "firemelon" "bapple"
      Left "key not in table: firemelon"

    The 'Maybe' and 'Either c' monads are good for writing database
    queries that may succeed or fail, but what if we want to write
    queries that may return many answers. In this case, we can use the
    List monad. -}


{-    PART 3 : LISTS, or DOING SEARCH WITH MONADS -}

{- In Lecture 4, we saw how list comprehensions can be used to write
   programs that look like database queries. Now we'll look at another
   way of writing these, using the "do notation".

   Using 'Monad'-like ideas to express database queries has recently
   been used in several industrial systems for database access. For
   example, Microsoft's LINQ uses monad ideas under the hood to
   structure how queries are embedded in C# and F#. See the article
   http://queue.acm.org/detail.cfm?id=2024658 for more details.

   Before we can use lists as a 'Monad', we need to define instances
   for our three typeclasses (these are already defined in the Haskell
   standard library, we redefined them here for illustration
   purposes).

   We saw the 'Functor' instance for Lists in Lecture 10: -}

instance Functor [] where
  -- fmap :: (a -> b) -> [a] -> [b]
  fmap = map

{- 'Applicative' for lists is defined so that 'pure' returns a singleton
   list, and '<*>' works by taking a list of functions and a list of
   arguments and doing every single way of combining a function and
   its argument. We've written this out usingexplicit recursion, and
   also using a list comprehension, which might be easier to
   understand. -}

instance Applicative [] where
  -- pure :: a -> [a]
  pure x = [x]
  
  -- (<*>) :: [a -> b] -> [a] -> [b]
  []     <*> xs = []
  (f:fs) <*> xs = fmap f xs ++ (fs <*> xs)

  -- List comprehension version:
  -- fs <*> xs = [ f x | f <- fs, x <- xs ]

{- The 'Monad' instance's '>>=' is given a list of inputs and a function
   that will produce a list of outputs for every input. So we apply
   the function to every input to get a list of lists, and then we
   concatenate all these lists to get a list of values. We've written
   a version using explicit recursion first, and then ones using
   'concat' and 'map', and then using list comprehensions. -}

instance Monad [] where
  -- return :: a -> [a]
  return = pure
  
  -- (>>=) :: [a] -> (a -> [b]) -> [b]
  []     >>= f = []
  (x:xs) >>= f = f x ++ xs >>= f

  -- Concat-map version:
  --   xs >>= f = concat (map f xs)
  --
  -- List comprehension version:
  --   xs >>= f = [ y | x <- xs, y <- f x ]

{- In addition to being a monad, lists also support two operations:
   'empty' and 'union'. We'll introduce 'union' below. 'empty' is used
   to represent the query that returns no results. It will be useful
   when we need to handle the case when our query's conditions are not
   met. 'empty' is similar in spirit to 'abort' in the 'Maybe' monad. -}

empty :: [a]
empty = []

{- With the list monad, and 'empty', we can write queries on databases
   that are represented as lists of records. Here is a query that
   searches for fruits that are more nutritious than some
   threshold. In the 'if-then-else' at the end, we use 'empty' to
   represent return of no results. -}

allFruitBetterThan :: Int -> [String]
allFruitBetterThan threshold = do
  (fruit, calories) <- fruitNutrition
  if calories >= threshold then
    return fruit
  else
    empty

{- Compare this to the SQL query:

      SELECT Fruit
        FROM FruitNutrition
       WHERE Calories > <threshold>

   When writing programs using the list monad, the pattern

      if <condition> then
         <something>
      else
         empty

   Comes up very often. So we turn this into a function we call
   'guard': -}

guard :: Bool -> [()]
guard True  = return ()
guard False = empty

{- Let's use 'guard' to write another query. This time we take a fruit
   name as a parameter, and search for all fruits that are more
   nutritious that that fruit. We use 'guard' to enforce our query's
   constraints: -}

allFruitBetterThanNamed :: String -> [String]
allFruitBetterThanNamed name = do
  (fruit, calories) <- fruitNutrition
  (fruit', calories') <- fruitNutrition
  guard (fruit == name && calories' >= calories)
  return fruit'

{- Compare this to the SQL query:

      SELECT F2.Fruit
        FROM FruitNutrition as F1,
             FruitNutrition as F2
       WHERE F1.Fruit = <name>
         AND F2.Calories >= F1.Calories

   The second operation for the list monad is 'union', which takes two
   lists of results of a query and concatenates them: -}

union :: [a] -> [a] -> [a]
union = (++)

{- Here is an example of the use of 'union' to combine results of two
   queries: -}

allFruitBetterThanAppleOrOrange :: [String]
allFruitBetterThanAppleOrOrange =
  allFruitBetterThanNamed "apple"
  `union`
  allFruitBetterThanNamed "orange"

{- Compare this to the SQL query:

      SELECT F2.Fruit
        FROM FruitNutrition as F1,
             FruitNutrition as F2
       WHERE F1.Fruit = "apple"
         AND F2.Calories >= F1.Calories
      UNION
      SELECT F2.Fruit
        FROM FruitNutrition as F1,
             FruitNutrition as F2
       WHERE F1.Fruit = "orange"
         AND F2.Calories >= F1.Calories



   Unlike SQL, we are not limited to searching tables that are stored in
   the database. For example, we can search arbitrary ranges of
   numbers to find ones with properties that we want.

   A "Pythagorean Triple" is a triple of positive whole numbers x, y,
   z such that x^2 + y^2 = z^2. That is, they could be lengths of the
   sides of a right-angled triangle. Using the List monad, we can
   write a program to search for all the "small" Pythagorean triples
   by treating the numbers 1 to 10 as a kind of "database table": -}

triples :: [(Integer,Integer,Integer)]
triples = do
  x <- [1..10]
  y <- [1..10]
  z <- [1..10]
  guard (x*x + y*y == z*z)
  return (x,y,z)

{- A special feature of lists is that we can also use the list
   comprehension notation that we saw in Lecture 4. Here is 'triple'
   again, but written using list comprehensions. The main difference
   is that list comprehensions are "upside down" -- the thing we are
   returning is written at the top instead of at the bottom. -}

triples' :: [(Integer, Integer, Integer)]
triples' = [ (x,y,z) | x <- [1..10]
                     , y <- [1..10]
                     , z <- [1..10]
                     , x*x + y*y == z*z ]

{-    EXERCISE: Rewrite 'allFruitBetterThan' and
      'allFruitBetterThanNamed' using list comprehensions. -}

{-    PART 4 : Other examples of Monads

   In this lecture, we looked at three examples of 'Monad's:

     1. 'Maybe', which can be used to represent values along with the
        possibility of failure.

     2. 'Either', which can be used to represent values along with the
        possibility of failure with an attached reason.

     3. Lists, which can be used to represent multiple values, or possibly none.

   In the rest of the course, we will see several other examples of 'Monad's:

     - In Exercise 3, the 'CP' type is a 'Monad'. The 'sequ' function
       we ask you to implement is the '>>=' for this monad.

     - In Lectures 14 and 15, we will cover parsing as a kind of
       monad.

     - In Lecture 16, we will cover 'Monad's for reading, writing, and
       state (i.e. imperative programming).

     - In Lecture 18, we will look at the special 'IO' monad, which is
       how Haskell programs communicate with the outside world.

   'Monad's are everywhere! -}
