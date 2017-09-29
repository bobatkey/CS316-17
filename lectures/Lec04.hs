{-# LANGUAGE ParallelListComp #-}

module Lec4 where

import Prelude hiding (concat,zip,lookup)
import Data.List.Split (splitOn)
import Data.List (nub)
-- (we are going to redefine some functions from the standard library)

{- LECTURE 04 : LIST COMPREHENSIONS   -}


{-  PART 1: COMPREHENSIONS -}

{- Different ways to construct sets/lists:

      [2,4,6,8]


      {2,4,6,8}

      {2,4,..,120}

      [2,3..120]

      { x*2 | x \in {0,1,2,3,4} }

      [ x * 2 | x <- [0,1,2,3,4] ]

-}














squares :: [Int]
squares = [ x ^ 2 | x <- [0..10] ]





-- There can be more than one generator:

allpairs :: [(Int,Int)]
allpairs = [ (x, y) | y <- [4..6], x <- [0..5] ]

-- Later generators can depend on the
-- values of earlier ones:

ordpairs :: [(Int,Int)]
ordpairs = [ (x, y) | x <- [1..3], y <- [x..5] ]

-- this gives a neat way to write concat:

concat :: [[a]] -> [a]
concat xss = [ x | xs <- xss, x <- xs]

-- Like on the LHS of equations, if a variable is
-- not used, it can be ignored with an
-- underscore:

firsts :: [(a,b)] -> [a]
firsts ps = [ x | (x , _) <- ps ]

firsts' ps = [ fst xy | xy <- ps ]







-- Guards, guards!

factors :: Int -> [Int]
factors n = [ x | x <- [1..n]
                , n `mod` x == 0 ]

prime :: Int -> Bool
prime 1 = True
prime n = factors n == [1,n]

primes :: [Int]
primes = [ x | x <- [1..], prime x ]

numberedPrimes :: [(Int,Int)]
numberedPrimes = [ (i, p) | p <- primes
                          , i <- [1..] ]


-- Zipping two lists together
-- (redefined here using the ParallelListComp
--  LANGUAGE extension):

-- (there is also a down-to-earth
-- pattern-matching definition)

zip :: [a] -> [b] -> [(a,b)]
zip []     _      = []
zip _      []     = []
zip (x:xs) (y:ys) = (x,y):(zip xs ys)

zip' :: [a] -> [b] -> [(a,b)]
zip' xs ys = [ (x, y) | x <- xs | y <- ys]

numberedPrimesProperly :: [(Int, Int)]
numberedPrimesProperly = zip' [1..] primes







-- Here's a mindblowing example if using
-- zip/parallel comprehensions:

-- recall fibs = [1,1,2,3,5,8,...]

fibs :: [Int]
fibs = 1:1:[ x + y | x <- fibs |
                     y <- tail fibs]
















{- PART 2: USING LIST COMPREHENSIONS AS A
           POOR MAN'S DATABASE -}

lookup :: Eq a => a -> [(a,b)] -> [b]
lookup k t = [ b | (a,b) <- t
                 , a == k ]

-- SELECT b
--   FROM t
--  WHERE a = k

-- Example modified from Simon Thompson:
-- 'Haskell: The Craft of Functional Programming'

type Person = String
type Book = String
type Fee  = Integer

type Database = [ (Person, Book, Fee) ]

exampleDB :: Database
exampleDB = [("Alice", "Tintin", 1)
            ,("Anna","Little Women", 2)
            ,("Alice","Asterix", 5)
            ,("Rory","Tintin", 0)
            ]

books :: Database -> Person -> [Book]
books db per = [ book |
                  (per, book, _) <- db ]
                --, per == per' ]

--lateBooks :: Database -> [(Book,Person)]
lateBooks db =
   [ per | (per, book, fee) <- db,
                  fee > 0 ]














{- Joining two files in a hacky way in ghci -}


-- Two files from https://data.glasgow.gov.uk/:
-- birth.csv and death.csv
-- (data from Glasgow 2012)









{-
-- joining births and deaths
sort [ (name ++ " " ++ zone, b, d) | [zone, name, d] <- death, [zone', b, _, _] <- birth, zone == zone' ]

-- areas dying out (in 2012)
sort [ (name ++ " " ++ zone, b, d) | [zone, name, d] <- death, [zone', b, fb, mb] <- birth, zone == zone', (read b :: Int) < read d ]



-}

printline = (\ (n,b,d) -> putStrLn $ n ++ ":" ++ (replicate (45 - length n) ' ') ++ b ++ "   \t" ++ d)
