module Lec16 where

import Prelude hiding (print)
import Control.Applicative hiding (some, many)
import Control.Monad
import Data.Char

{-    LECTURE 16 : PARSING, WRITING, AND THE STATE -}

{- In this lecture we explore more examples of monads, and how to use
   them for programming. We begin by using the Parser monad from the
   previous two lectures to parse a little language of expressions,
   and then move on to introduce three more monads: the so-called
   writer, state and reader monads. -}

{-    PART 0 : PARSING EXPRESSIONS -}

{- Here's the infrastructure for monadic parsing that we have introduced
   so far: -}

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
  empty = MkParser (\_ -> Nothing)

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

string :: String -> Parser ()
string []     = pure ()
string (e:es) = (\() () -> ()) <$> isChar e <*> string es

-- zero or more
many :: Parser a -> Parser [a]
many p =  (\ a as -> a:as) <$> p <*> many p
      <|> pure []

-- one or more
some :: Parser a -> Parser [a]
some p = (\ a as -> a:as) <$> p <*> many p


{- Using these combinators, we can start to build up more advanced
   parsers. For example, here is a parser which also accepts some
   surrounding whitespace: -}

withSpaces :: Parser a -> Parser a
withSpaces p = (\ () a () -> a) <$> spaces <*> p <*> spaces
  where spaces = ignore (many (satisfies isSpace))

{- Parsing numbers and strings is all very well, but Exercise 4 is going
   to be about parsing a programming language. Let's start small by
   writing a parser for a small language of numbers and addition.
   (This language is often called Hutton's Razor, since it the
   simplest non-trivial example of a language, introduced by Graham
   Hutton). -}

data Expr
  = Add    Expr Expr
  | Number Int
  deriving Show

{- The final goal is to parse strings that look like

       "2+(4+5)+1"

   into the corresponding value

       Add (Number 2) (Add (Add (Number 4) (Number 5)) (Number 1))

   where we have resolved the ambiguity caused by a lack of
   parentheses by nesting to the right.

   We first need a parser for numbers, and this is easily produced: -}

number :: Parser Int
number = foldl (\acc d -> d + 10*acc) 0 <$> some digit

digit :: Parser Int
digit = do
  c <- char
  if isNumber c then
    return (digitToInt c)
  else
    empty

{-
   Thinking about our definition of expressions as a grammar, our
   first attempt might be:

      E ::= E '+' E
          | n
          | '(' E ')'

   (ignoring direction of nesting for now). We can attempt to
   translate this directly into our 'Parser's as follows: -}

expr_v1 :: Parser Expr
expr_v1 =  (\ e1 () e2 -> Add e1 e2) <$> expr_v1 <*> isChar '+' <*> expr_v1
       <|> Number                    <$> number
       <|> (\_ e _ -> e)             <$> isChar '(' <*> expr_v1 <*> isChar ')'

{- This definition looks very elegant, and apart from the additional
   action parts to translate the parse results into 'Expr' trees, it
   looks very close to the grammar. Unforunately, it doesn't work:

       λ> runParser expr_v1 "2+(4+5)+1"
       *** Exception: stack overflow

   The problem is that the original grammar contains /left
   recursion/. The second case for 'E' immediately refers to 'E' again
   (reading left-to-right), without consuming any symbols. In terms of
   our definition of 'expr_v1' this amounts to the following sequence
   of decisions made:

   1. To parse an 'expr_v1', there are three choices.

   3. The first choice is to parse "E '+' E", so we call 'expr_v1' to
   parse an 'E', but this means we go back to (1) again, with the same
   input.

   Hence, we get stuck in a loop and eventually crash with a stack
   overflow exception. Changing the order of the alternatives does
   not help, because the other alternatives will fail to parse an
   expression involving '+'.

   The fix to this problem is to make sure that there are no calls
   back to the same parser without making progress through the
   input. A crude way of ensuring this is to demand that all
   expressions are completely bracketed. So we will only accept input
   like:

       "(2+((4+5)+1))"

   We do this like so, asking that each '+' expression is wrapped in
   parentheses: -}

parens :: Parser a -> Parser a
parens p = (\ () a () -> a) <$> isChar '(' <*> p <*> isChar ')'

expr_v2 :: Parser Expr
expr_v2 =  parens ((\ e1 () e2 -> Add e1 e2) <$> expr_v2
                                             <*> isChar '+'
                                             <*> expr_v2)
       <|> Number                            <$> number

{- Now we have

       λ> runParser expr_v2 "(2+((4+5)+1))"
       [Add (Number 2) (Add (Add (Number 4) (Number 5)) (Number 1))]

   as required. But putting in all those parentheses is annoying. We
   can refactor the grammar into 'E'xpressions and 'B'ase
   expressions. This resolves the ambiguity, and the left recursion
   problems:

     E ::= B
         | B '+' E

     B ::= n
         | '(' E ')'
-}


expr_v3 :: Parser Expr
expr_v3 =  (\ e1 _ e2 -> Add e1 e2) <$>
              base_v3 <*> isChar '+' <*> expr_v3
       <|> base_v3

base_v3 :: Parser Expr
base_v3 =  Number <$> number
       <|> parens (expr_v3)

{- Example uses:

       λ> runParser expr_v3 "(1+2+3)"
       Just (Add (Number 1) (Add (Number 2) (Number 3)),"")
       λ> runParser expr_v3 "1+2+3"
       Just (Add (Number 1) (Add (Number 2) (Number 3)),"")
       λ> runParser expr_v3 "(1+2)+3"
       Just (Add (Add (Number 1) (Number 2)) (Number 3),"")
-}





{-    PART 1 : PRINTING, or THE WRITER MONAD -}

{- We now turn to other examples of monads. Back in Lecture 12, we
   considered an evaluator for a language which included print
   statements. There, we realised that "type expressions with a
   printing side-effect", represented as product types (String, a),
   formed an applicative functor, which we exploited to tidy up the
   definition of the evaluator. Now, we will see that this is in fact
   also a monad, which can be used to write programs with a logging
   effect.

   Here is the type of pairs of 'String's and 'a's again, this time
   represented as a named data type for clarity: -}

data Printing a = MkPr String a

{- We can define a Show instance for Printing a that first prints the
   log messages, then a dividing line, and then shows the value: -}

instance Show a => Show (Printing a) where
  show (MkPr msg a) = msg ++ "\n=====\n\n" ++ (show a)

{- Just like in Lecture 12, Printing is a Functor and an applicative: -}

instance Functor Printing where
  -- fmap :: (a -> b) -> Printing a -> Printing b
  fmap f (MkPr msg a) = MkPr msg (f a)

instance Applicative Printing where
  -- pure :: a -> Printing a
  pure a = MkPr "" a

  -- (<*>) :: Printing (a -> b) -> Printing a -> Printing b
  (MkPr str1 f) <*> (MkPr str2 a) = MkPr (str1 ++ str2) (f a)

{- EXERCISE: What structure on strings are we using to define an
     Applicative instance for Printing?

   With not much further difficulty, we can also define the following
   Monad instance, which again combines the log string from the first
   argument with the log string produced by 'f a': -}

instance Monad Printing where
  -- (>>=) :: Printing a -> (a -> Printing b) -> Printing b
  (MkPr str1 a) >>= f = let (MkPr str2 b) = f a in (MkPr (str1 ++ str2) b)

{- This monad supports the following operation: -}

print :: String -> Printing ()
print s = MkPr s ()

{- Let's use it to define the following convenience function, e.g. for
   debugging: -}

trace :: Show a => String -> a -> Printing ()
trace msg v = print $ msg ++ ": " ++ (show v) ++ "\n"

{- Here is an example of its use: we will take the standard
   (inefficient) implementation of the Fibonacci function -}

fibStd :: Integer -> Integer
fibStd n = if n == 0 || n == 1 then 1
              else fibStd (n-1) + fibStd (n-2)

{- and add a trace for each call, to get a feeling for exactly how
   inefficient it is. Here is the version of fibStd in the Printing
   monad: -}

fib :: Integer -> Printing Integer
fib n = do
  trace "fib" n
  if n == 0 || n == 1 then pure 1
    else (+) <$> fib (n-1) <*> fib (n-2)

{-  Notice the similarities with the pure version 'fibStd' above: except
    for the call to 'trace', and some extra punctuation from the
    applicative interface, the program looks almost the same.

    We could also write this in an even more imperative style as
    follows: -}

fib' :: Integer -> Printing Integer
fib' n = do
  trace "fib" n
  if n == 0 || n == 1 then return  1
  else do
    m <- fib' (n - 1)
    n <- fib' (n - 2)
    return (m + n)

{- Here is a small example of running the program:

       λ> fib 4
       fib: 4
       fib: 3
       fib: 2
       fib: 1
       fib: 0
       fib: 1
       fib: 2
       fib: 1
       fib: 0

       =====

       5

    And here is a large example:

       λ> fib 25
       fib: 25
       fib: 24
       fib: 23
       fib: 22
       fib: 21
       fib: 20
       fib: 19
       fib: 18
       fib: 17
       fib: 16
       [...242769 lines omitted...]
       fib: 0
       fib: 3
       fib: 2
       fib: 1
       fib: 0
       fib: 1

       =====

       121393
-}

{-   PART 2 : THE STATE MONAD -}

{- The Printing monad makes it possible to record output, but not to
   read input. We will now introduce a monad that also let's us do
   this; this monad is called the state monad, since the ability to
   read and write means that we can now write stateful programs.

   In an imperative programming language such as Java or Python, every
   program can potentially change the state of the program.

   In Haskell, we can encapsulate the stateful computations, and keep
   track of what is stateful using types. Here is the type of values
   that make use of some state:
-}

newtype State s a = MkState (s -> (a,s))

{- We think of a function

      f :: a -> State s b

   as a function that

   - takes an input of type a

   - produces an output of type b

   - using and modifying local state of type s

   The state monad supports the following two operations: -}

get :: State a a
get = MkState (\ a -> (a,a))

put :: s -> State s ()
put s = MkState (\ _ -> ((), s))

{- With the reading above, the types of 'get' and 'put' says that 'get'
   is a computation that returns a value of type a, using local state
   of type a, and 'put' is a function which when given an input of
   type s will manipulate state of type s, and produce a (trivial)
   output of type ().

   If we start with some initial state, we can run the whole stateful
   computation to produce a "pure" result: -}

evalState :: State s a -> s -> a
evalState h = fst . runState h

runState :: State s a -> s -> (a,s)
runState (MkState h) s = (h s)

{- How do we chain stateful computations together? State s is a monad! -}

instance Functor (State s) where
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap f (MkState h) =
    MkState (\s -> let (a,s') = h s in (f a, s'))

instance Applicative (State s) where
  -- pure :: a -> State s a
  pure x = MkState (\s -> (x,s))

  -- <*> :: State s (a -> b) -> State s a -> State s b
  (MkState hg) <*> (MkState ha) =
    MkState (\s -> let (g, s1) = hg s in
                   let (a, s2) = ha s1 in
                   (g a, s2))

instance Monad (State s) where
  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  (MkState ha) >>= f =
    MkState (\s -> let (a, s1) = ha s in
                   let MkState h = f a in
                     h s1)

{- These definitions are a bit fiddly; we see that the <*> and >>=
   instances carefully threads the state through the computation. The
   good news is that now after we have defined them, we can use them
   freely without worrying about threading the state through the
   computation ourselves.

   Here is an example program that is much easier to write using local
   state. It again operates on binary trees: -}

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

{- Our mission is to take a tree, and replace each value with a running
 counter. We store the current value of the counter in the state. -}

labelTree :: Tree a -> State Int (Tree Int)
labelTree Leaf = return Leaf
labelTree (Node l _ r) = do
  l' <- labelTree l
  n <- get
  put (n+1)
  r' <- labelTree r
  return (Node l' n r')

{- We can now turn this stateful computation into a pure one by
   supplying an initial state: -}

lTree :: Tree a -> Tree Int
lTree t = evalState (labelTree t) 0

{- Here's a tree to try it out on: -}

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

{- Running labelling this tree results in the following:

       λ> lTree bigTree
       Node (Node (Node (Node (Node Leaf 0 Leaf) 1 Leaf) 2 Leaf) 3 (Node (Node Leaf 4 (Node Leaf 5 (Node Leaf 6 Leaf))) 7 (Node (Node Leaf 8 Leaf) 9 Leaf))) 10 Leaf
-}


{-    PART 3 : READING, with THE READER MONAD -}

{- Sometimes we want to pass around some values to our programs, without
   modifying them. In this case, the state monad is overkill. As one
   might expect, there is also a monad which allows read-only access
   to some data, usually called the Reader monad. The type definition
   is as follows: -}

data Reader r a = MkR (r -> a)

runReader :: Reader r a -> r -> a
runReader (MkR h) r = h r

{- Here are the instance declarations. -}

instance Functor (Reader r) where
  fmap f (MkR g) = MkR (f . g)

instance Applicative (Reader r) where
  -- pure :: a -> Reader r a
  pure x = MkR (\ _ -> x)

  -- (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (MkR rf) <*> (MkR ra) = MkR (\ x -> rf x (ra x))

instance Monad (Reader r) where
  -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (MkR ra) >>= f = MkR (\ x -> let MkR rb = f (ra x) in rb x)

{- In addition, the reader monad supports the following operation: -}

ask :: Reader a a
ask = MkR (\x -> x)

{- Compare with 'get :: State a a'!

   Here is a typical use case of the reader monad: we have some
   options or environment values that we have populated somewhere, and
   now many different parts of our program need access to them. We
   could pass them around as additional arguments to our functions, or
   we could use the reader monad to do this boilerplate argument
   passing for us.

   Consider the following simple type of environment options:
-}

data Environment = Opt { username :: String, isSuperuser :: Bool }

{- An example environment value is the following: -}

env0 :: Environment
env0 = Opt "harry" False

{- Let's say that we want to compute the path to a user's home directory
   on a Unix system. Using the reader monad, we can do it as follows: -}

homeDir :: Reader Environment String
homeDir = do
  env <- ask
  if isSuperuser env then
    return "/root"
  else
    return $ "/home/~" ++ (username env)

{- For small examples such as this, the point of using a reader monad is
   perhaps not so clear. The reader monad starts to become useful when
   the size of the program grows.

   We will see the reader monad used in disguise in Exercise 4. -}
