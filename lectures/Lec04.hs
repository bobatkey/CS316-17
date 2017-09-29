{-# LANGUAGE ParallelListComp #-}

module Lec4 where

import Prelude hiding (concat,zip,lookup)
import Data.List.Split (splitOn)
import Data.List (nub)
-- (we are going to redefine some functions from the standard library)

{- LECTURE 04 : LIST COMPREHENSIONS   -}


{-  PART 1: COMPREHENSIONS -}

{- Different ways to construct sets/lists:



-}













squares :: [Int]
squares = undefined


















-- There can be more than one generator:

allpairs :: [(Int,Int)]
allpairs = undefined






















-- Later generators can depend on the values of
-- earlier ones:

ordpairs :: [(Int,Int)]
ordpairs = undefined













-- this gives a neat way to write concat:

concat :: [[a]] -> [a]
concat xss = undefined












-- Like on the LHS of equations, if a variable is
-- not used, it can be ignored with an
-- underscore:

firsts :: [(a,b)] -> [a]
firsts ps = undefined









-- Guards, guards!

factors :: Int -> [Int]
factors n = undefined

prime :: Int -> Bool
prime n = undefined

primes :: [Int]
primes = undefined












-- Zipping two lists together
-- (redefined here using the ParallelListComp
--  LANGUAGE extension):

zip :: [a] -> [b] -> [(a,b)]
zip xs ys = undefined

-- (there is also a down-to-earth
-- pattern-matching definition)









-- Here's a mindblowing example if using
-- zip/parallel comprehensions:

-- recall fibs = [1,1,2,3,5,8,...]

fibs :: [Int]
fibs = undefined
















{- PART 2: USING LIST COMPREHENSIONS AS A POOR
           MAN'S DATABASE -}

lookup :: Eq a => a -> [(a,b)] -> [b]
lookup k t = undefined

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
books db per = undefined


lateBooks :: Database -> [(Book,Person)]
lateBooks db = undefined















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
