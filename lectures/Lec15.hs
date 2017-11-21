module Lec15 where

import Control.Applicative hiding (some, many)
import Control.Monad
import Data.Char

{-     LECTURE 15 : MORE MONADIC PARSING -}

{- First we'll recap the 'Parser' type we defined in the last lecture.

   Parsers of things are functions from strings to the possibility of
   pairs of things and strings: -}

newtype Parser a = MkParser (String -> Maybe (a,String))

{- We apply a 'Parser' to a string using 'runParser', which returns
   either 'Nothing' if the parser fails to extract anything from the
   string, or 'Just (a, s)' if it extracted 'a' with leftover string
   's'. We'll see more examples of this below. -}

runParser :: Parser a -> String -> Maybe (a, String)
runParser (MkParser p) input = p input

{- What makes 'Parser's so useful is that they support lots of
   structure: 'Functor', 'Applicative', 'Alternative', and 'Monad'. We
   introduced this structure in the previous lecture, so we briefly
   reintroduce it here.

   The 'Functor' typeclass includes the 'fmap' function, which allows
   us to post-process the results of a 'Parser', as we saw last time. -}

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (MkParser p) =
    MkParser (\input -> fmap (\(a,rest) -> (f a,rest)) (p input))

{- The 'Alternative' instance for 'Parser's allows us to try one parser
   and then another if the first one fails ('p1 <|> p2'), or to write
   a parser that always fails ('empty'). -}

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

{- The 'Applicative' instance for 'Parser's allows us to run one parser
   and then another one afterwards. The leftover input from the first
   parser is fed into the second one. -}

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

{- The 'Monad' instance allows us to run a 'Parser' and then choose
   another 'Parser' based on the data parsed by the first. This is
   useful for making decisions based on the input, such as filtering
   out certain things, as we saw in the previous lecture. -}

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

{- Finally, we have a 'Parser' that only succeeds when we are at the end
   of input (eoi). -}
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

{- We'll also use the function 'ignore', which uses 'fmap' to
   post-process the result of a parser than returns an 'a' to throw it
   away and just return '()' on success. -}

ignore :: Parser a -> Parser ()
ignore p = fmap (\_ -> ()) p

{- Using 'satisfies' and 'ignore', we can write a parser than recognises
   left parentheses '(': -}

leftParen :: Parser ()
leftParen = ignore (satisfies (\x -> x == '('))

{- Let's see it working:

       λ> runParser leftParen "("
       Just ((),"")
       λ> runParser leftParen ")"
       Nothing
       λ> runParser leftParen "(abc"
       Just ((),"abc")

   In the same way, we can write a parser that only recognises left
   square brackets '[': -}

leftSqBracket :: Parser ()
leftSqBracket = ignore (satisfies (\x -> x == '['))

{- Using the same pattern, we can write many useful parsers. Here's one
   that recognises whitespace: spaces, newlines, tabs, and carriage
   returns: -}

space :: Parser ()
space = ignore (satisfies (\x -> x == ' ' || x == '\n' || x == '\t' || x == '\r'))

{- Alternatively, we can use the built-in function 'isSpace' from the 'Data.Char' module (imported above):

     space = ignore (satisfies isSpace)

   Another useful little parser is the one that is given a specific
   character 'c', and succeeds only if the first character in the
   input is 'c': -}

isChar :: Char -> Parser ()
isChar c = ignore (satisfies (\x -> x == c))

{- Using 'isChar', we can use it repeatedly to build a parser that
   recognises a given string: -}

string :: String -> Parser ()
string []     = nothing -- pure ()
string (e:es) = (\() () -> ()) <$> isChar e <*> string es

{- For example:

       λ> runParser (string "hello") "hello!!!"
       Just ((),"!!!")

   The function 'string' parses more than one character by recursing
   over the sequence of characters it has been told to look for. But
   what if we don't know what we are looking for in advance. How can
   we run a parser repeatedly until it fails? -}


{-     PART II. Repeated Parsing

   
-}


-- zero or more
many :: Parser a -> Parser [a]
many p =  (\a as -> a:as) <$> p <*> many p 
      <|> pure []

-- fmap f s = pure f <*> s
--          = f <$> s

{- Examples using 'many':

      λ> runParser (many space) ""
      Just ([], "")
      λ> runParser (many space) "    "
      Just ([(),(),(),()], "")
      λ> runParser (many char) "    "
      Just ("    ","")
      λ> runParser (many char) " sdhgsjfhksdh   "
      Just (" sdhgsjfhksdh   ","")

   
-}

-- one or more
some :: Parser a -> Parser [a]
some p = (\ a as -> a:as) <$> p <*> many p

{- Examples using 'some'

     λ> runParser (some space) ""
     Nothing
     λ> runParser (some space) " "
     Just ([()], "")
-}


{- Things separated by other things -}

sepBy :: Parser () -> Parser a -> Parser [a]
sepBy separator thing =
      (:) <$> thing
          <*> many ((\ () t -> t) <$> separator <*> thing)
  <|> pure []



{- Extended example: Writing a CSV parser -}

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

