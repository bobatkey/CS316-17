module Lec06 where

import Prelude hiding (map, filter, reverse, (++), (.))

{-    LECTURE 06 : HIGHER ORDER FUNCTIONS

   In this lecture, we talked about the concept of functions as
   values, and how this can be used to turn programs that solve one
   specific problem into more general programs that solve whole
   classes of problems.

   Haskell programs can pass values like integers and strings to and
   from functions, and store them in structures like lists and
   trees. Haskell treats functions no differently from any other kind
   of data: functions can be returned as the result of functions,
   passed into functions, and stored in data structures.

   First, we will look at how functions can return functions as
   results. -}

{-    PART I : FUNCTIONS THAT RETURN FUNCTIONS

   We've already seen many functions that take several arguments. An
   example is 'add', which adds two 'Int's and returns an 'Int': -}

add :: Int -> Int -> Int
add x y = x + y

{- We write the type of a function that takes two arguments like so:

        t1 -> t2 -> t3

   What we've not mentioned so far is that this is really shorthand
   notation for the following type with parentheses inserted:

        t1 -> (t2 -> t3)

   Remembering that 'a -> b' is the type of functions that take 'a's
   and return 'b's, we can read this type as the type of "functions
   that take 't1's and return functions that take 't2's and return
   't3's.

   Therefore, the add function "takes an 'Int' and returns a function
   that takes an(other) 'Int' and returns an 'Int'.

   Once we see that 'add' is really a function that returns a
   function, we can see that we needn't always give it two
   arguments. We can define the 'addTen' functions by only giving
   'add' one of its arguments: -}

addTen :: Int -> Int
addTen = add 10

{- 'addTen' has type 'Int -> Int', even though we didn't write an
   argument name on the left side of the '='s, because 'add' has type
   'Int -> (Int -> Int)' and we've given an 'Int', leaving 'Int ->
   Int'. We could also write 'addTen' giving an explicit name for the
   argument, which we pass on to 'add 10'. This gives 'addTen2', which
   is equivalent to 'addTen': -}

addTen2 :: Int -> Int
addTen2 x = add 10 x

{- We can see even more clearly that multi-argument functions in Haskell
   work by taking one argument and returning a function by writing out
   a definition of 'add' using the '\x -> E' notation for
   functions. (The backslash '\' is meant to be an ASCII
   representation of a Greek lambda, because 'lambda' is a commonly
   used mathematical notation for writing anonymous functions.) An
   expression of the form '\x -> E' stands for "a function that takes
   an argument, which we call 'x', and returns 'E'". We write out
   'add' in this form like so: -}

add2 :: Int -> (Int -> Int)
add2 = \x -> (\y -> x + y)

{- (Look at the way the bracketing in the program matches the bracketing
   in the type!)

   As a shorthand, we can avoid writing things like "\x -> (\y -> (\z ->
   ..." and instead write all the argument names together before the
   "->": -}

add3 :: Int -> (Int -> Int)
add3 = \x y -> x + y

{- The `\`/lambda notation also accepts patterns as well as argument
   names, as long as there is only one pattern. For example, pattern
   matching against pairs: -}

fst2 :: (a,b) -> a
fst2 = \(a,b) -> a

{- (Look at the coincidence between the type and the program!)

   The '\'/lambda notation for functions may seem a bit pointless so
   far. Everything we've written using this notation could have been
   written more simply by placing the argument names to the left of
   the '='s. The advantage of the '\'/lambda notation is that it
   allows us to write functions without needing to give them
   names. We'll see why this is important after we look at functions
   that take other functions as input. -}


{-     PART II : FUNCTIONS THAT TAKE FUNCTIONS AS INPUT

   As I said in the introduction, Haskell treats functions as it does
   any other kind of value. The can be returned by functions, as we
   saw in Part I. We'll now look at how and why Haskell functions can
   take functions as arguments.

   Let's start by looking at a simple definition. Here is a definition
   of the number 'ten' by adding '5' to itself: -}

ten :: Int
ten = add 5 5

{- We now think to ourselves "there's nothing special about the number
   '5' here, we could be adding any number to itself". So we move
   from the specific '5' to a general 'x', which we make an argument
   of the function. We now have a function that takes an 'Int' and
   returns an 'Int': -}

double :: Int -> Int
double x = add x x

{- Continuing this line of thought, we think to ourselves "there's
   nothing special about 'add'ing here, we could use any operation
   that takes two 'Int's and returns an 'Int'". So we move from the
   specific 'add' to a general 'f', which we make an argument of the
   function. We adjust the type again: 'add' has type 'Int -> Int ->
   Int', so our new function takes a value of this type and returns a
   function that takes 'Int's and returns 'Int's: -}

applyCopy :: (Int -> Int -> Int) -> Int -> Int
applyCopy f x = f x x

{- 'applyCopy' is now a generally applicable function that takes /any/
   two argument function on 'Int's, and /any/ 'Int' and passes that
   'Int' twice to the given function.

   We call 'applyOrder' a *higher order* function because it takes a
   function as its argument. The order of a function refers to how
   'functiony' its arguments are: A value with no arguments is of
   order 0, a function with arguments that have no arguments is order
   1, a function with arguments that take arguments is order 2, and so
   on.

   Because we have constructed 'applyCopy' by repeated moves from the
   specific to the general, we can get back down to earth again by
   applying 'applyCopy' to the original specific 'add' and '5'. So we
   can recover the 'double' function by applying 'applyCopy' to 'add': -}

double2 :: Int -> Int
double2 = applyCopy add

{- And we can recover 'ten' by applying 'double2' to '5': -}

ten2 :: Int
ten2 = double2 5

{- When we moved from 'ten' to 'applyCopy' above, we didn't change the
   types much: in the end, 'applyCopy' still worked on 'Int's. In the
   example, we will see how moving from specific functions to more
   general ones allows us to also make the types more general too.

   The 'quadruple' function applies 'double' twice to double a number: -}

quadruple :: Int -> Int
quadruple x = double (double x)

{- As above, there is nothing special about 'double' here. We move from
   the specific 'double' to a general 'f' to make the 'twice'
   function, which applies a function to an argument, and then applies
   it again. Only this time, we also make the type more general --
   there is nothing specific to 'Int's in the definition of 'twice' so
   we can replace the specific 'Int' by the general 'a': -}

twice :: (a -> a) -> a -> a
twice f x = f (f x)

{-    EXERCISE: what is the more general type for 'applyCopy' above?

   We needn't always work out the more general type for ourselves. We
   can ask GHCi to do it for us:

      Prelude> :t twice
      (t -> t) -> t -> t

   As with 'applyCopy' above, we can recover 'quadruple' by applying
   twice to 'double', or any other way of writing 'double' that we can
   think of. This is where the anonymous '\'/lambda notation comes in
   very useful for writing short functions that are only mentioned
   once without needing to think of a name. We can write 'double' as
   '\x -> x + x', which in some contexts may be clearer than the word
   'double'. -}

quadruple2 :: Int -> Int
quadruple2 = twice (applyCopy add)
      --     twice double
      --     twice (\x -> x + x)
      --     twice (\x -> 2 * x)

{- Because 'twice' is more general than 'quadruple', we can use it again
   for new purposes. For example, octupling:-}

octtuple :: Int -> Int
octtuple = twice quadruple

{- FIXME: another example of twice. -}

{- One of the most useful places to use higher-order functions is to
   make general functions for processing containers full of
   data. Here, we will concenrate on lists. Let's see how to make some
   reusable functions on lists by following the same
   specific-to-general methodology that we did above.

   Here is a function that doubles every element of a list of
   integers: -}

doubleAll :: [Int] -> [Int]
doubleAll []     = []
doubleAll (x:xs) = double x : doubleAll xs

{- As above, we not that there is nothing special about use of the
   'double' function here. So we can move from the specific 'double'
   to the general 'f'. This gives us a general function that applies
   'f' to every element of a list, giving a new list of transformed
   elements. The name 'map' is the traditional name for this function: -}

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

doubleAll2 :: [Int] -> [Int]
doubleAll2 = map double

fsts :: [(a,b)] -> [a]
fsts = map fst


filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

onlyEvens :: [Int] -> [Int]
onlyEvens = filter (\x -> x `mod` 2 == 0)


compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

{-
    grep CS316 registered-students.txt | cut -f1
-}

pipeline :: [(String,Int)] -> [String]
pipeline = fsts . filter (\(s,i) -> s == "CS316")

{-
  grep CS316 registered-students.txt | wc -l
-}

pipeline2 :: [(String,Int)] -> Int
pipeline2 = length . filter (\(s,i) -> s == "CS316")

everyOther :: [a] -> [a]
everyOther = map snd . filter (\ (i,x) -> i `mod` 2 == 1) . zip [0..]

--     zip               filter (...)              map snd
-- [a] ---> [(Int, a)] ----------------> [(Int,a)] --------> [a] 

everyOther2 :: [a] -> [a]
everyOther2 =  map snd . filter fst . zip (cycle [False, True])
