module Tut07 where

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

-- things surrounded by parentheses
parens :: Parser a -> Parser a
parens p = (\_ a _ -> a) <$> isChar '(' <*> p <*> isChar ')'


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
