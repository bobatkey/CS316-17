module Lec15 where

import Control.Applicative hiding (some, many)
import Control.Monad
import Data.Char

{- LECTURE 15 : MORE MONADIC PARSING -}

{- Parsers of things are functions from strings to the possibility of
   pairs of things and strings: -}

newtype Parser a = MkParser (String -> Maybe (a,String))



runParser :: Parser a -> String -> Maybe (a, String)
runParser (MkParser p) input = p input

instance Monad Parser where
  -- return :: a -> Parser a
  return x = MkParser (\input -> Just (x, input))

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  MkParser p >>= f =
    MkParser (\input -> do
                 (a, input2) <- p input
                 let MkParser p2 = f a
                 p2 input2)

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = pure f <*> p

instance Applicative Parser where
  --pure :: a -> Parser a
  pure = return

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = pf >>= \f -> pa >>= \a -> return (f a)

instance Alternative Parser where
  -- empty :: Parser a
  empty = MkParser (\input -> empty)

  -- (<|>) :: Parser a -> Parser a -> Parser a
  MkParser p1 <|> MkParser p2 =
    MkParser (\input -> p1 input <|> p2 input)

{- So far, we haven't yet defined anything that actually consumes any
   input. We do this by defining the 'char' parser. This reads a
   single character from the input, or fails if that is not possible. -}
char :: Parser Char
char = MkParser (\input -> case input of
                             ""   -> Nothing
                             c:cs -> Just (c,cs))

eoi :: Parser ()
eoi = MkParser (\input -> case input of
                            ""  -> Just ((), "")
                            _:_ -> Nothing)

{--------------------------------------------------------------------}
{- Part 1. Building parsers                                         -}
{--------------------------------------------------------------------}

{- We have now defined all of the basic functions we need to build more
   complex parsers. Everything to do with parsers from this point on
   is done in terms of:

     1. The Monad interface  (return, >>=)
     2. The Applicative interface (pure, <*>)
     3. The Alternative interface (empty, <|>)
     4. 'char'
     5. 'eoi'

   And, of course, to actually use parsers we need the 'runParser'
   function.

   Let's see now how to build up more complex parsers to recognise
   more complex kinds of input.

   The 'char' parser accepts any input, and returns it. Sometimes we
   will want to make sure that the character read matches some
   criteria. For example, we might want to make sure it is an opening
   parenthesis '(', or it is a digit.

   We define 'satisfies' in terms of 'char', the Monad structure '>>='
   and 'return', and part of the Alternative structure 'empty': -}

satisfies :: (Char -> Bool) -> Parser Char
satisfies predicate =
  char >>= \c ->
  if predicate c then return c else empty

{- Examples:

       λ> runParser (satisfies (== '(')) "("
       Just ('(', "")
       λ> runParser (satisfies (== '(')) ")"
       Nothing

   Often, we will want to ignore the results of a parser. We do this
   using the 'ignore' function: -}

ignore :: Parser a -> Parser ()
ignore p = const () <$> p
           -- pure (\a -> ()) <*> p -- Parser (a -> ()) -> Parser a -> Parser ()
           -- p >>= \a -> return ()

{- FIXME: several ways to write this now -}

-- Whitespace
space :: Parser ()
space = ignore (satisfies isSpace)


-- Literal characters
isChar :: Char -> Parser ()
isChar c = ignore (satisfies (==c))

-- isChar '('


-- zero or more
many :: Parser a -> Parser [a]
many p =  (:)     <$> p <*> many p
      <|> pure []

-- fmap f s = pure f <*> s
--          = f <$> s

{- Examples using 'many':

     λ> runParser (many space) ""
     Just ([], "")
     λ> runParser (many space) "    "
     Just ([(),(),(),()], "")
     λ> runParser (many char) "    "
     ["    "]
     λ> runParser (many char) " sdhgsjfhksdh   "
     [" sdhgsjfhksdh   "]
-}

-- one or more
some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p

{- Examples using 'some'

     λ> runParser (some space) ""
     []
     λ> runParser (some space) " "
     [[()]]
-}


-- literal strings
string :: String -> Parser ()
string []     = pure ()
string (e:es) = (\_ _ -> ()) <$> isChar e <*> string es

-- things separated by other things
sepBy :: Parser () -> Parser a -> Parser [a]
sepBy separator thing =
      (:) <$> thing
          <*> many (const id <$> separator <*> thing)
  <|> pure []


  -- 1,2,3
  -- a,b,c

  -- sepBy (isChar ',') char

{- Parsing comma separated values:

     λ> runParser (sepBy (isChar ',') char) "1,2,3"
     ["123"]
     λ> runParser (sepBy (isChar ',') char) "1,2,,3"
     []
     λ> runParser (sepBy (isChar ',') char) ",1,2,,3"
     []
     λ> runParser (sepBy (isChar ',') char) ",1,2,3"
     []
     λ> runParser (sepBy (isChar ',') char) "1,2,,,"
     []
     λ> runParser (sepBy (isChar ',') char) "1,2,,"
     ["12,"]
     λ> runParser (sepBy (isChar ',') char) "1,2,,,3"
     ["12,3"]
     λ> runParser (sepBy (isChar ',') (satisfies (/=','))) "1,2,,,3"
     []
     λ> runParser (sepBy (isChar ',') (satisfies (/=','))) "1,2,3,3"
     ["1233"]
     λ> runParser (sepBy (isChar ',') (some (satisfies (/=',')))) "1,2,3,3"
     [["1","2","3","3"]]
     λ> runParser (sepBy (isChar ',') (some (satisfies (/=',') <|> const ',' <$> string "\\,"))) "1,2,3,3"
     [["1","2","3","3"]]
     λ> runParser (sepBy (isChar ',') (some (satisfies (/=',') <|> const ',' <$> string "\\,"))) "1,2,3\\,3"
     [["1","2","3\\","3"],["1","2","3,3"]]
     λ> runParser (sepBy (isChar ',') (some (satisfies (\c -> c/=','||c/='\\') <|> const ',' <$> string "\\,"))) "1,2,3\\,3"
     [["1","2","3\\","3"],["1","2","3\\,3"],["1","2","3,3"],["1","2,3\\","3"],["1","2,3\\,3"],["1","2,3,3"],["1,2","3\\","3"],["1,2","3\\,3"],["1,2","3,3"],["1,2,3\\","3"],["1,2,3\\,3"],["1,2,3,3"]]
     λ> runParser (sepBy (isChar ',') (some (satisfies (\c -> c/=','&&c/='\\') <|> const ',' <$> string "\\,"))) "1,2,3\\,3"
     [["1","2","3,3"]]
-}

-- things surrounded by parentheses
parens :: Parser a -> Parser a
parens p = (\_ a _ -> a) <$> isChar '(' <*> p <*> isChar ')'


{--------------------------------------------------------------------}
{- Part 2. Parsing Expressions                                      -}
{--------------------------------------------------------------------}

data Expr
  = Number Int
  | Add    Expr Expr
  deriving Show

-- FIXME: use a proper number parser

{- A grammar for expressions:

    E ::= E '+' E
        | n

   Unfortunately, this grammar is ambiguous. The string:

      1+1+1

   Can be parsed in two different ways.

      (1+1)+1
      1+(1+1)

   Another problem is that the grammar uses left recursion. The second
   case for 'E' immediately mentions 'E' without moving through the
   input. This means that if we try to turn this grammar into a parser
   directly, we will fail: -}

expr_v1 :: Parser Expr
expr_v1 =  (\e1 _ e2 -> Add e1 e2) <$> expr_v1 <*> isChar '+' <*> expr_v1
       <|> Number                  <$> (const 1 <$> some (satisfies isDigit))
{-
     λ> runParser expr_v1 "1+1"
     *** Exception: stack overflow
-}

{- One way to fix this is to just demand that there are always
   parentheses around every expression that isn't a number, using the
   'parens' function. This means we are parsing the following grammar:

     E ::= n
         | '(' E '+' E ')'
-}

expr_v2 :: Parser Expr
expr_v2 = Number <$> (const 1 <$> some (satisfies isDigit))
       <|> parens ((\e1 _ e2 -> Add e1 e2) <$> expr_v2 <*> isChar '+' <*> expr_v2)

{-
     λ> runParser expr_v2 "(1+1)"
     [Add (Number 1) (Number 1)]
     λ> runParser expr_v2 "(1+(1+1))"
     [Add (Number 1) (Add (Number 1) (Number 1))]
     λ> runParser expr_v2 "(1+1+1)"
     []
-}

{- Putting in all those parentheses is annoying, so we can refactor the
   grammar into 'E'xpressions and 'B'ase expressions. This resolves
   the ambiguity, and the left recursion problems:

     E ::= B
         | B '+' E

     B ::= n
         | '(' E ')'
-}

expr_v3 :: Parser Expr
expr_v3 = base_v3
       <|> Add <$> base_v3 <* isChar '+' <*> expr_v3

{- In this definition, I have used the function '<*' to throw away the
   results of parsing the '+' symbol. The '<*' function has the
   following type, compared to the more normal '<*>' function:

       (<*)  :: Parser a -> Parser b -> Parser a
       (<*>) :: Parser (a -> b) -> Parser a -> Parser b
-}

base_v3 = Number <$> (const 1 <$> some (satisfies isDigit))
       <|> parens (expr_v3)

{- Example uses:

       λ> runParser expr_v3 "(1+1+1)"
       [Add (Number 1) (Add (Number 1) (Number 1))]
       λ> runParser expr_v3 "1+1+1"
       [Add (Number 1) (Add (Number 1) (Number 1))]
       λ> runParser expr_v3 "(1+1)+1"
       [Add (Add (Number 1) (Number 1)) (Number 1)]
       λ> runParser expr_v3 "(1+1)+1"
       [Add (Add (Number 1) (Number 1)) (Number 1)]
-}
