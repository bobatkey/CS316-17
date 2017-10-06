module Lec06 where

import Prelude hiding (map, filter, reverse, (++), (.))

{-    LECTURE 06 : HIGHER ORDER FUNCTIONS


-}

{-    PART I : FUNCTIONS THAT RETURN FUNCTIONS -}

add :: Int -> Int -> Int
add x y = x + y

addTen :: Int -> Int
addTen = add 10

addTen2 :: Int -> Int
addTen2 x = add 10 x

add2 :: Int -> (Int -> Int)
add2 = \x y -> x + y

-- \x y (z1,z2) -> E

fst2 :: (a,b) -> a
fst2 = \(a,b) -> a

-- foo :: Int -> (A -> (B -> (C -> D)))



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
