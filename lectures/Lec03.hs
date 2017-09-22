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
myVal = Foo

--   with parameters

baz :: MyType -> MyType
baz x = Bar (Bar x)



--   using conditional expression (e.g., not)

not0 :: Bool -> Bool
not0 x = if x then False else True  {-  x ? false : true -}

gcd0 :: Int -> Int -> Int
gcd0 x y = if x == y then x
           else if x < y then gcd0 x (y - x)
                else gcd0 (x - y) y



{- Fred: -}

-- replacing conditionals on the right by guards on the left
--   e.g., greatest common divisor (gcd is already a thing, so rename)

gcd1 :: Int -> Int -> Int
gcd1 x y | x == y = x
gcd1 x y | x < y  = gcd1 x (y - x)
gcd1 x y | otherwise = gcd1 (x - y) y

not1 :: Bool -> Bool
not1 x | x = False
not1 _     = True



{- Bob: -}

-- pattern matching
--   e.g., not not again? (underscore patterns)

not2 :: Bool -> Bool
not2 True  = False
not2 False = True

--   e.g., maybeApply, maybe?  (:info Maybe)

maybeApply :: Maybe (a -> b) -> Maybe a -> Maybe b
maybeApply (Just f) (Just a) = Just (f a)
maybeApply _        _        = Nothing

{- Fred: -}

-- pattern matching on lists
--   e.g., append
--   e.g., reverse (the slow way)
--   e.g., sawPrefixOff

-- deduce append from generalising examples
--   do []
--   do (_:_)

-- append [1,2,3] [4,5,6] = [1,2,3,4,5,6]

-- append [] [4,5,6] = [4,5,6]

-- append [1,2,3] [4,5,6] = [1,2,3,4,5,6] = [1, "append [2,3] [4,5,6]"] 

append :: [x] -> [x] -> [x]
append []     ys = ys
append (x:xs)   ys      =   x : (append xs ys)
--     [1,2,3]  [4,5,6]     1   [2,3,4,5,6]

-- rev [] 
-- rev (_:_)

-- rev [4,5,6] = [6,5,4] 
-- rev [1,2,3,4] = [4,3,2,1]


rev :: [x] -> [x]
rev [] = []
rev (x:xs) = append (rev xs) [x]


-- sawPrefix [1,2,3] [1,2,3,4,5,6] = [4,5,6]
-- sawPrefix [1,2,3] [1,2]         = ?

-- 

sawPrefixOff :: Eq a => [a] -> [a] -> Maybe [a]
sawPrefixOff []     ys     = Just ys
sawPrefixOff (x:xs) []     = Nothing
sawPrefixOff (x:xs) (y:ys) = if x == y then sawPrefixOff xs ys else Nothing

-- basename ".txt" "mylifestory.txt" == Just "mylifestory"

basename :: String -> String -> Maybe String
basename ext filename = case sawPrefixOff (rev ext) (rev filename) of
                          Nothing -> Nothing
                          Just str -> Just (rev str)

-- (hint: need a Maybe!)

hd :: [a] -> Maybe a
hd []     = Nothing
hd (x:xs) = Just x
