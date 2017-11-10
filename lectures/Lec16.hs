module Lec16 where

import Prelude hiding (print)
import Control.Applicative hiding (some, many)
import Control.Monad
import Data.Char

import System.Environment

{-    LECTURE 16 : PARSING, WRITING, AND THE STATE -}

{-    PART 0 : PARSING EXPRESSIONS -}

{- Parsers of things are functions from strings to the possibility of
   pairs of things and strings: -}

newtype Parser a = MkParser (String -> Maybe (a,String))

runParser :: Parser a -> String -> Maybe (a, String)
runParser (MkParser p) input = p input

instance Functor Parser where
  fmap f p =
    MkParser (\input -> fmap (\(a,rest) -> (f a,rest)) (runParser p input))

instance Applicative Parser where
  pure x = MkParser (\ input -> Just (x , input))

  pf <*> pa =
    MkParser (\ input -> case runParser pf input of
                           Nothing        -> Nothing
                           Just (f, rest) -> case runParser pa rest of
                             Nothing         -> Nothing
                             Just (a, rest') -> Just (f a, rest'))

instance Alternative Parser where
  empty = MkParser (\input -> Nothing)

  p1 <|> p2 =
    MkParser (\input -> case runParser p1 input of
                          Nothing    -> runParser p2 input
                          Just (a,s) -> Just (a,s))


instance Monad Parser where
  p >>= f =
    MkParser (\input -> case runParser p input of
                          Nothing -> Nothing
                          Just (a,rest) -> runParser (f a) rest)

-- Basic parsers

char :: Parser Char
char = MkParser (\input -> case input of
                             ""   -> Nothing
                             c:cs -> Just (c,cs))

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = do
  c <- char
  if p c then return c else empty

ignore :: Parser a -> Parser ()
ignore p = fmap (\_ -> ()) p

isChar :: Char -> Parser ()
isChar c = ignore (satisfies (\x -> x == c))

-- zero or more
many :: Parser a -> Parser [a]
many p =  (\ a as -> a:as) <$> p <*> many p
      <|> pure []

-- one or more
some :: Parser a -> Parser [a]
some p = (\ a as -> a:as) <$> p <*> many p


withSpaces :: Parser a -> Parser a
withSpaces p = (\ () a () -> a) <$> spaces <*> p <*> spaces
  where spaces = ignore (many (satisfies isSpace))




-- Parsing Expressions

data Expr
  = Add    Expr Expr
  | Number Int
  deriving Show

{- A grammar:

     E ::= E '+' E
         | n

   Two problems:
   1. The grammar is ambiguous: should 1 + 2 + 3 be parsed as

         (1 + 2) + 3

       or

         1 + (2 + 3)


    2. It uses left recursion: the first case for 'E' immediately
       mentions 'E' without moving through the input.

    This means that a direct translation of the grammar will fail:

 -}

expr_v1 :: Parser Expr
expr_v1 =  undefined


number :: Parser Int
number = undefined



{- One way to fix this is to just demand that there are always
   parentheses around every expression that isn't a number.

   This means we are parsing the following grammar:

     E ::= '(' E '+' E ')'
         | n
-}

-- things surrounded by parentheses
parens :: Parser a -> Parser a
parens p = undefined




expr_v2 :: Parser Expr
expr_v2 =  undefined







{- Putting in all those parentheses is annoying, so we can refactor the
   grammar into 'E'xpressions and 'B'ase expressions. This resolves
   the ambiguity, and the left recursion problems:

     E ::= B '+' E
         | B

     B ::= n
         | '(' E ')'
-}

expr_v3 :: Parser Expr
expr_v3 =  (\ e1 _ e2 -> Add e1 e2) <$>
              base_v3 <*> isChar '+' <*> expr_v3
       <|> base_v3

base_v3 =  Number <$> number
       <|> parens (expr_v3)



{-    PART 1 : PRINTING, or THE WRITER MONAD -}

data Printing a = MkPr String a

instance Show a => Show (Printing a) where
  show (MkPr msg a) = msg ++ "\n=====\n\n" ++ (show a)

instance Functor Printing where
  -- fmap :: (a -> b) -> Printing a -> Printing b
  fmap f pa = undefined

instance Applicative Printing where
  -- pure :: a -> Printing a
  pure a = undefined

  -- (<*>) :: Printing (a -> b) -> Printing a -> Printing b
  (MkPr str1 f) <*> (MkPr str2 a) = undefined

instance Monad Printing where
  -- (>>=) :: Printing a -> (a -> Printing b) -> Printing b
  (MkPr str1 a) >>= f = undefined


-- The printing monad supports this operation:
print :: String -> Printing ()
print s = undefined


trace :: Show a => String -> a -> Printing ()
trace msg v = print $ msg ++ ": " ++ (show v) ++ "\n"






fib :: Integer -> Printing Integer
fib n = undefined






{-   PART 2 : STATE -}

-- Printing was writing only, but what if we also want to read?
-- Reading and writing means keeping track of some state!



-- In Java, every program can potentially change the state.
-- In Haskell, we can encapsulate the stateful computations, and
--   keep track of what is stateful using types.


newtype State s a = MkState (s -> (a,s))


-- A function
--
--   f :: a -> State s b
--
-- is a function that
--   * takes an     input of type a
--   * produces an output of type b
--   * using local state of type s

-- The state monad supports the following two operations:

get :: State a a
get = MkState (\ a -> (a,a))

put :: s -> State s ()
put s = MkState (\ _ -> ((), s))






-- If we start with some initial state, we can run the
-- whole stateful computation to produce a "pure" result:

evalState :: State s a -> s -> a
evalState h = fst . runState h

runState :: State s a -> s -> (a,s)
runState (MkState h) s = (h s)






-- How do we chain stateful computations together? State s is a monad!

instance Functor (State s) where
  fmap f (MkState h) =
    undefined

instance Applicative (State s) where
  pure x = undefined

  (MkState hg) <*> (MkState ha) =
    undefined

instance Monad (State s) where
  (MkState ha) >>= f = undefined



-- An example program using state:

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

-- Take a tree, and replace each value with a running
-- counter.

labelTree :: Tree a -> State Int (Tree Int)
labelTree Leaf = undefined









bigTree :: Tree String
bigTree = Node
            (Node
               (Node
                  (Node
                     (Node Leaf "b" Leaf)
                     "a"
                     Leaf)
                  "c"
                  Leaf)
               "j"
               (Node
                 (Node
                   Leaf
                   "d"
                   (Node
                     Leaf
                     "e"
                     (Node Leaf "f" Leaf)))
                 "g"
                 (Node
                   (Node Leaf "h" Leaf)
                   "i"
                   Leaf)))
            "k"
            Leaf


{-    PART 3 : READING, with THE READER MONAD -}

data Reader r a = MkR (r -> a)

runReader :: Reader r a -> r -> a
runReader (MkR h) r = h r

instance Functor (Reader r) where
  fmap f (MkR g) = MkR (\ r -> f (g r))

instance Applicative (Reader r) where
  -- pure :: a -> Reader r a
  pure x = MkR (\ _ -> x)

  -- (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (MkR rf) <*> (MkR ra) = MkR (\ x -> rf x (ra x))

instance Monad (Reader r) where
  -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (MkR ra) >>= f = MkR (\ x -> let MkR rb = f (ra x) in rb x)


-- The Reader monad supports the following operation:

ask :: Reader a a
ask = MkR (\x -> x)






data Environment = Opt { username    :: String
                       , isSuperuser :: Bool
                       }
  deriving Show

homeDir :: Reader Environment String
homeDir = undefined




env0 :: Environment
env0 = Opt "harry" False
