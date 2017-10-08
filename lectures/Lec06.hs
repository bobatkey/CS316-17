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


{-    PART II : FUNCTIONS THAT TAKE FUNCTIONS AS INPUT -}



ten :: Int
ten = add 5 5

double :: Int -> Int
double x = add x x

applyCopy :: (Int -> Int -> Int) -> Int -> Int
applyCopy f x = f x x

double2 :: Int -> Int
double2 = applyCopy add

ten2 :: Int
ten2 = double2 5


quadruple :: Int -> Int
quadruple x = double (double x)

twice :: (a -> a) -> a -> a
twice f x = f (f x)

twice2 :: (a -> a) -> a -> a
twice2 f = \x -> f (f x)

quadruple2 :: Int -> Int
quadruple2 = twice (applyCopy add)
      --     twice double
      --     twice (\x -> x + x)
      --     twice (\x -> 2 * x)

doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (x:xs) = double x : doubleAll xs

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
