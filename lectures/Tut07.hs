module Tut07 where

{- TUTORIAL 07 : PARSING EXPRESSIONS -}

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
{- Part 2. Parsing Expressions                                      -}
{--------------------------------------------------------------------}

digit :: Parser Int
digit =
  char >>= \c ->
  if isNumber c then
    pure (digitToInt c)
  else
    empty

number :: Parser Int
number = foldl (\acc d -> acc*10 + d) 0 <$> some digit


data Expr
  = Number Int
  | Add    Expr Expr
  deriving Show



{- A grammar:

     E ::= E '+' E
         | n
-}

