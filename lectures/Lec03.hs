module Lec03 where

{-    LECTURE 03 : DEFINING FUNCTIONS

-}


{- Bob: -}

-- declarations versus definitions
--   a declaration introduces new types or values
--   a definition gives a new way to compute existing values

-- declaration

data MyType = Foo | Bar MyType deriving Show

-- definitions

-- definition by equation
--   without parameters

myVal :: MyType
myVal = undefined

--   with parameters

baz :: MyType -> MyType
baz x = undefined



--   using conditional expression (e.g., not)

not0 :: Bool -> Bool
not0 x = undefined

gcd0 :: Int -> Int -> Int
gcd0 = undefined -- with conditionals



{- Fred: -}

-- replacing conditionals on the right by guards on the left
--   e.g., greatest common divisor (gcd is already a thing, so rename)

gcd1 :: Int -> Int -> Int
gcd1 = undefined

not1 :: Bool -> Bool
not1 = undefined



{- Bob: -}

-- pattern matching
--   e.g., not not again? (underscore patterns)

not2 :: Bool -> Bool
not2 = undefined

--   e.g., maybeApply, maybe?  (:info Maybe)

maybeApply :: Maybe (a -> b) -> Maybe a -> Maybe b
maybeApply = undefined

{- Fred: -}

-- pattern matching on lists
--   e.g., append
--   e.g., reverse (the slow way)
--   e.g., sawPrefixOff

-- deduce append from generalising examples
--   do []
--   do (_:_)

-- append [1,2,3] [4,5,6] = [1,2,3,4,5,6]
-- append []       ys = ys
-- append (x : xs) ys = x : append xs ys

append :: [x] -> [x] -> [x]
append = undefined

-- rev []
-- rev (_:_)

rev :: [x] -> [x]
rev = undefined

-- sawPrefix [1,2,3] [1,2,3,4,5,6] = [4,5,6]
-- sawPrefix [1,2,3] [1,2]         = ?

sawPrefixOff :: Eq a => [a] -> [a] -> [a]
sawPrefixOff = undefined

-- (hint: need a Maybe!)

hd :: [a] -> Maybe a
hd = undefined
