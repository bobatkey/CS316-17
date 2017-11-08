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

instance Functor Parser where
  fmap f (MkParser p) =
    MkParser (\input -> fmap (\(a,rest) -> (f a,rest)) (p input))


andThen :: Parser a -> Parser b -> Parser (a,b)
andThen (MkParser p1) (MkParser p2) =
  MkParser (\input -> case p1 input of
                        Nothing -> Nothing
                        Just (a, input2) ->
                          case p2 input2 of
                            Nothing -> Nothing
                            Just (b, rest) ->
                              Just ((a,b), rest))
nothing :: Parser ()
nothing = MkParser (\input -> Just ((), input))

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = const x <$> nothing

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = (\(f, a) -> f a) <$> (pf `andThen` pa)

orElse :: Parser a -> Parser a -> Parser a
orElse (MkParser p1) (MkParser p2) =
  MkParser (\input -> case p1 input of
                        Nothing -> p2 input
                        Just (a,s) -> Just (a,s))

failure :: Parser a
failure = MkParser (\input -> Nothing)

instance Alternative Parser where
  empty = failure
  (<|>) = orElse

instance Monad Parser where
  MkParser p >>= f =
    MkParser (\input -> case p input of
                          Nothing -> Nothing
                          Just (a,rest) ->
                            let MkParser p2 = f a in
                              p2 rest)

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

     1. The Functor interface (fmap)
     2. The Monad interface  (return, >>=)
     3. The Applicative interface (pure, <*>)
     4. The Alternative interface (empty, <|>)
     5. 'char'
     6. 'eoi'

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
satisfies p = do
  c <- char
  if p c then return c else empty

leftParen :: Parser ()
leftParen = ignore (satisfies (\x -> x == '('))

leftSqBracket :: Parser ()
leftSqBracket = ignore (satisfies (\x -> x == '['))

ignore :: Parser a -> Parser ()
ignore p = fmap (\_ -> ()) p


-- Whitespace
space :: Parser ()
space = ignore (satisfies (\x -> x == ' ' || x == '\n' || x == '\t' || x == '\r'))

-- alternatively:
--  space = ignore (satisfies isSpace)

-- Literal characters
isChar :: Char -> Parser ()
isChar c = ignore (satisfies (\x -> x == c))

-- literal strings
----  runParser (string "hello") "hello!!!" = Just ((), "!!!")

string :: String -> Parser ()
string []     = nothing -- pure ()
string (e:es) = (\() () -> ()) <$> isChar e <*> string es





-- zero or more
many :: Parser a -> Parser [a]
many p = (\ a as -> a:as) <$> p <*> many p 
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
some p = (\ a as -> a:as) <$> p <*> many p

{- Examples using 'some'

     λ> runParser (some space) ""
     []
     λ> runParser (some space) " "
     [[()]]
-}


-- things separated by other things
sepBy :: Parser () -> Parser a -> Parser [a]
sepBy separator thing =
      (:) <$> thing
          <*> many ((\ () t -> t) <$> separator <*> thing)
  <|> pure []

field :: Parser String
field =  (\_ s _ -> s) <$> isChar '\"'
                       <*> many (satisfies (\x -> x /= '\"' && x /= '\n'))
                       <*> isChar '\"'
     <|> many (satisfies (\x -> x /= ':' && x /= '\n'))

unescapedChar :: Parser Char
unescapedChar =
  satisfies (\x -> x /= '\"' && x /= '\n' && x /= '\\')

escapedChar :: Parser Char
escapedChar =
  (\_ c -> c) <$> isChar '\\' <*> char

field2 :: Parser String
field2 =  (\_ s _ -> s) <$> isChar '\"'
                        <*> many (unescapedChar <|> escapedChar)
                        <*> isChar '\"'
      <|> many (satisfies (\x -> x /= ':' && x /= '\n'))

{-
field' = MkParser myFieldParser
  where myFieldParser input =
          _
-}
  -- 1  ,2 ,3
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

