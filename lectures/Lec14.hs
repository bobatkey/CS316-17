module Lec14 where

import Data.List
import Data.Char
import Control.Applicative

{- LECTURE 14 : MONADIC PARSING

  This lecture is about parsers. What is a parser?

   A parser of things is---
     a function
       from strings
         to lists
           of pairs
             of things
               and strings!

    -- Fritz Ruehr, channeling Dr. Suess.

  Let's turn this poem into a type: -}

newtype Parser a = -- A parser of things is
  MkParser (String ->    -- a function from Strings
            [( a         -- to lists of pairs of things
             , String)]) -- and Strings.

{- The idea is that a parser takes a string as input, and returns a
   collection of possible ways to turning that string into results as
   well as whatever is left of the input.

   Here's an example of what we mean by left-over input. If we have a
   parser that counts the number of 'A's at the start of a string then
   on the input "AAAA" we would have the following possible results:

     [ (0, "AAAA"), (1, "AAA"), (2, "AA"), (3, "A"), (4, "") ]

   The need to return left-over input may not be obvious at first --
   if the goal is to count the number of 'A's in the string, then why
   bother only counting some of them and returning the rest? The
   answer will come later when we want to run parsers in sequence.

   For now though, we write a function that runs parsers and extracts
   all the parses that result in no left-overs. We will use this
   function to test our parsers. -}

runParser :: Parser a -> String -> [a]
runParser (MkParser p) input = [ a | (a,"") <- p input ]

{- 'runParser' has been written using a list comprehension: the pattern
   match specifies that we are only interested in the results returned
   by the parser that have no left-over input. -}

{------------------------------------------------------------}
{- Part 1. Parsing one thing or another.                    -}

{- 'runParser' lets us apply a parser to a string, but we need ways to
   build parsers. A straightforward but tedious way to build parsers
   is to build them by hand, using pattern matching on lists to
   extract information. Here is a parser written in this style that
   parses the input "Bob" into the value '()': -}
bob :: Parser ()
bob = MkParser recogniseBob
  where recogniseBob ('B':'o':'b':rest) = [((), rest)]
        recogniseBob _                  = []

{- The definition of 'bob' shows two important points about our parsers:

   1. We signal a parse success by returning a list with something in
   it. In this case we return the value '()' as the result of parsing
   'Bob', and the 'rest' of the input.

   2. We signal failure to parse by returning the empty list '[]'.

   We test this parser by running it on some inputs:

       λ> runParser bob "Bob"
       [()]
       λ> runParser bob "Ben"
       []
       λ> runParser bob "BobBob"
       []

   From the first example, we see that parsing success is represented
   by a non-empty list of results. The second and third examples show
   how parse failure is represented, either as a result of unexpected
   input ("Ben") or left-over input at the end ("BobBob").

   Parsing a single fixed string is not so interesting. Let's write a
   parser that parses the names of the staff members teaching this
   course this semester. First we make a datatype that represents all
   the possible names: -}

data CS316Staff = Ben | Bob | Conor | Fred | James
  deriving (Show, Eq)

{- We could now write a separate parser for each of the strings "Ben",
   "Conor", "Fred", and "James" in the same style as we did for "Bob"
   above. But this would lead to lot of repeated work. The parser
   'bob' above represented a common pattern when writing a parser: the
   case when we want to match the input against an expected
   string. This kind of pattern is common when parsing keywords in a
   programming language, for example.

   We can capture this pattern with the following function
   'string'. The parser 'string x' is a parser that recognises the
   string 'x' and nothing else. The body of the parser (defined in the
   'where' clause) match the expected input against the real input
   step by step until we run out of expected input. -}

string :: String -> Parser ()
string expected = MkParser (p expected)
  where
    p :: String -> String -> [((), String)]
    p []     rest               = [((), rest)]
    p _      []                 = []
    p (e:es) (c:cs) | e == c    = p es cs
                    | otherwise = []

{- We test our new 'parser generator' on a few examples:

       λ> runParser (string "Bob") "Bob"
       [()]
       λ> runParser (string "Ben") "Ben"
       [()]
       λ> runParser (string "Ben") "Bob"
       []
       λ> runParser (string "Bob") "Ben"
       []

   And we can use it to give ourselves a collection of parsers that
   recognise the names of the members of the CS316 team: -}

ben, conor, fred, james :: Parser ()
ben   = string "Ben"
conor = string "Conor"
fred  = string "Fred"
james = string "James"

{- We now have a way of recognising individual names. But what if we
   want to recognise strings that contain one of any of the names
   "Bob", "Ben", "Conor", "Fred", or "James"? We need a way of putting
   parsers together that means "try the input on the first one, and
   then try the same input on the second one". We do this with an
   'orElse' combinator: -}

orElse :: Parser a -> Parser a -> Parser a
orElse (MkParser p1) (MkParser p2) =
  MkParser (\input -> p1 input ++ p2 input)

{- 'orElse' takes two parsers 'p1' and 'p2' and an input 'input' and
   applies both parsers to 'input'. It then concatenates the results
   -- combining the possibilities from the first parser with the
   possibilities from the second. This is not the only way we could
   have combined the two results. Another interesting way is to only
   use the second parser if the first one returns no possible parses
   (represented by the empty list).

   Accompanying 'orElse' is the useful 'failure' combinator that
   represents the parser that always fails. It returns the empty list
   of parses. -}

failure :: Parser a
failure = MkParser (\input -> [])

{- Using 'orElse' we can build a parser that recognises the names of all
   members of the CS316 team: -}

cs316Staff_v1 :: Parser ()
cs316Staff_v1 = ben `orElse` bob `orElse` conor `orElse` fred `orElse` james

{- Testing 'cs316Staff_v1', we see that it accepts when it should, and
   rejects when it should:

       λ> runParser cs316Staff_v1 "Ben"
       [()]
       λ> runParser cs316Staff_v1 "Bob"
       [()]
       λ> runParser cs316Staff_v1 "Fred"
       [()]
       λ> runParser cs316Staff_v1 "Haskell"
       []
       λ> runParser cs316Staff_v1 "Simon"
       []

   But 'cs316Staff_v1' is unsatisfying. When it does recognise
   someone's name, it just returns '()'. It would be nicer if it would
   return the value from the 'CS316Staff' type representing that
   person.

   We could rewrite our 'string' combinator to take a value to return
   instead of '()' when the expected string is matched. But this would
   be a very special purpose fix. A more general fix is to notice that
   this kind of problem fits a pattern that we have seen before:

   1. We have values of type 'Parser ()': ben, bob, conor, fred, james.

   2. We want values of type 'Parser CS316Staff'.

   3. We can build functions of type '() -> CS316Staff', one for each
      of 'Ben', 'Bob', 'Conor', 'Fred', and 'James'.

   This is exactly the kind of situation that 'Functor' is meant to
   handle: we have a container (in this case a Parser) that contains
   'a's, and a way of getting from 'a's to 'b's, and we want a
   container full of 'b's.

   Writing a 'Functor' instance for 'Parser's is an exercise in
   following the types to burrow down to where the 'a's live and
   transform them into 'b's using the supplied function 'f': -}

instance Functor Parser where
  fmap f (MkParser p) =
    MkParser (\input -> map (\(a,rest) -> (f a,rest)) (p input))

{- Using our 'fmap' for 'Parser's, we can turn each of 'ben', 'bob',
   'conor', 'fred', and 'james' into 'Parser's that return the right
   value of CS316Staff on success:-}

ben', bob', conor', fred', james' :: Parser CS316Staff
ben' = fmap (\_ -> Ben) ben
bob' = fmap (\_ -> Bob) bob
{- We can make these definitions look a little nicer by using two
   pre-defined functions:

     1. The functions '(\_ -> Ben)' and '(\_ -> Bob)' are so-called
        "constant" functions because they always return the same
        value. The Haskell standard library already contains the
        function 'const' that build constant functions. So 'const
        Conor' is the function that always returns 'Conor', no matter
        what input it is given.

     2. 'fmap' can be written between the function and the argument
        using the symbol '<$>'. This makes the use of a functor look a
        little like function application.

   Using these two shortcuts, we define the informative versions of
   'conor', 'fred', and 'james': -}
conor' = const Conor <$> conor
fred'  = const Fred <$> fred
james' = const James <$> james

{- Chaining together all these parsers now gives us a parser that
   recognises the names of the CS316 team, and tells us which one it
   recognised: -}

cs316Staff :: Parser CS316Staff
cs316Staff =
  ben' `orElse` bob' `orElse` conor' `orElse` fred' `orElse` james'

{- Testing 'cs316Staff' shows it returns the right values:

       λ> runParser cs316Staff "Ben"
       [Ben]
       λ> runParser cs316Staff "Bob"
       [Bob]
       λ> runParser cs316Staff "Conor"
       [Conor]
       λ> runParser cs316Staff "Haskell"
       []
-}


{------------------------------------------------------------}
{- Part 2. Parsing one thing after another                  -}

{- The 'string' combinator allows us to parse fixed strings, and
   'orElse' allows us to parse input where there are multiple choices
   of input. The next useful feature of any parsing system is the
   ability to parse one thing followed by another thing.

   Before we do that, we make sure we can parse nothing -- the empty
   string: -}

nothing :: Parser ()
nothing = MkParser (\input -> [((), input)])

{- Remember that 'nothing' is different to 'failure'! 'failure' always
   fails to parse anything, while 'nothing' always succeeds, but never
   consumes any input. Effectively, 'nothing' is the parser that only
   recognises the empty input.

   EXERCISE: Write 'nothing' using the 'string' function defined above.

   Now we can parse nothing, we define a way to parse one thing after
   another. The function 'andThen' takes two parsers 'p1' and 'p2'. It
   feeds the input into 'p1', getting back a value 'a' and the
   leftover input 'rest0'. It then feeds 'rest0' into 'p2' to get
   another value 'b' and the leftover input 'rest'. The result of
   using both parsers is the pair '(a,b)' and the final leftover input
   'rest'. -}

andThen :: Parser a -> Parser b -> Parser (a,b)
andThen (MkParser p1) (MkParser p2) =
  MkParser (\input -> [ ((a,b), rest) | (a,rest0) <- p1 input
                                      , (b,rest)  <- p2 rest0 ])

{- Observe the difference in how the input is handled in 'andThen'
   compared to 'orElse'. In 'orElse', the same input is given to both
   parsers, and their outputs are concatenated. In 'andThen', the
   input is "threaded through" the parsers: it is first given to 'p1',
   and 'p1's output is given to 'p1'. This "threading" behaviour makes
   'Parser's very similar to the ST state monad in Lecture 12.

   Using 'andThen', we can define a parser that parses:
     - a name of a CS316 staff member
     - an ampersand ("&")
     - a name of a CS316 staff member
-}

lectureTeam_v1 :: Parser (CS316Staff, CS316Staff)
lectureTeam_v1 =
  (\((s1,_),s2) -> (s1,s2)) <$> (cs316Staff `andThen` string "&" `andThen` cs316Staff)

{- This definition splits into two halves:

   1. The bit before the '<$>' is often called the 'action' of the
      parser, that turns the results of sub-parsers into the final
      result. In this case, it uses pattern matching to extract the
      relevant parts to return.

   2. The bit after the '<$>' describes the items to parse, matching
      our specification above.

   The pattern matching on tuples is a little messy, and when we have
   more than three things after the '<$>' it can be quite noisy. The
   more standard way to define this kind of parser is to use the
   'Applicative' interface.

   We can define an instance of 'Applicative' for 'Parser's using the
   'nothing' and 'andThen' combinators we have already defined: -}

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = const x <$> nothing

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = (\(f, a) -> f a) <$> (pf `andThen` pa)

{- So 'pure x' is the parser that accepts nothing (the empty string) and
   always returns 'x'. We build this using 'nothing' to accept the
   empty string, and 'const x' and '<$>' to convert the result to 'x'.

   'pf <*> pa' is the parser that parses the input to get a function
   'f', parses the leftover input to get an argument 'a', and then
   applies 'f' to 'a'. The sequencing of 'pf' and 'pa' are handled by
   'andThen'.

   EXERCISE: Rewrite the 'pure' and '<*>' definitions directly,
   instead of using 'nothing' and 'andThen'.

   Using the Applicative interface, we can rewrite the 'action' part
   of our lecture team parser to be a little neater: -}

lectureTeam :: Parser (CS316Staff, CS316Staff)
lectureTeam =
  (\s1 _ s2 -> (s1,s2)) <$> cs316Staff <*> string "&" <*> cs316Staff

{- Testing:

       λ> runParser lectureTeam "Bob&Conor"
       [(Bob,Conor)]
       λ> runParser lectureTeam "Fred&Conor"
       [(Fred,Conor)]

   Defining an Applicative instance for 'Parser' means that we can
   also give the 'failure' and 'orElse' parsers above more standard
   names, using the 'Alternative' interface. We couldn't do this
   before because the Haskell standard library requires that every
   'Alternative' instance is also an 'Applicative' instance. -}

instance Alternative Parser where
  empty = failure
  (<|>) = orElse

{------------------------------------------------------------}
{- Part 3. Making Decisions based on parsed input.          -}

{- The 'lectureTeam' parser allows us to parse pairs of names that can
   teach a lecture. However, not all pairs of names can teach a
   lecture -- we shouldn't be able to have the same name twice! Can we
   make a parser that allows pairs where the two names are distinct,
   but rejects those that have the same name twice?

   We first write a function that identifies when we have a 'proper'
   lecture team -- one with two different people: -}

isProperPair :: (CS316Staff, CS316Staff) -> Bool
isProperPair (s1, s2) = s1 /= s2

{- Now our problem is use 'isProperPair' to filter out pairs that are
   not proper. One 'bruteforce' way of doing this is to exploit our
   representation of parsers as functions that return lists of
   results, and write a function with type:

       filterParse :: (a -> Bool) -> Parser a -> Parser a

   EXERCISE: Write such a function.

   A more revealing approach is to take a step back and think about
   what we are trying to do. We want to read some input (a pair of
   names), and based on that input we want to make a decision about
   what to do next: either return the pair, or to fail.

   This general pattern -- do task A, and then select a task B to do
   based on the result of doing task A, is the precise pattern
   captured by the Monad type class. 'Monad' extends 'Applicative'
   with the additional function:

     (>>=) :: m a -> (a -> m b) -> m b

   So 'c >>= f' can be read as: do 'c', and then, depending on the
   result 'a' of 'c', do 'f a'.

   'Parser's support this interface, which we demonstrate by
   implementing a 'Monad' instance: -}

instance Monad Parser where
  MkParser p >>= f =
    MkParser (\input -> [ (b, rest) | (a,rest0) <- p input
                                    , let MkParser p2 = f a
                                    , (b, rest) <- p2 rest0 ])

{- It is instructive to compare this definition to 'andThen' defined
   above. The similarity is that the input is "threaded" through: the
   input is given to 'p' and then the leftovers are given to the next
   parser. The difference lies in how the next parser is generated. In
   'andThen', the next parser was just given. In '(>>=)', the next
   parser is computed by the function 'f' from the result of the first
   parser. This allows the parser used for the rest of the input to
   depend on the first.

   Using the ability to let past results affect future parsing, we can
   write 'properLectureTeam', a parser that only accepts pairs of
   staff members with no repetitions: -}

properLectureTeam :: Parser (CS316Staff, CS316Staff)
properLectureTeam =
  lectureTeam >>= \pair ->
  if isProperPair pair then pure pair else failure

{------------------------------------------------------------}
{- Part 4. Parsing Numbers                                  -}

{- So far, we have combinators to:

   1. Match strings exactly: 'string'
   2. Translate parse results: 'fmap', '<$>'
   3. Make choices: 'empty' and '<|>' (introduced as 'failure' and 'orElse')
   4. Sequence: 'pure' and '<*>' (and also 'nothing' and 'andThen')
   5. Allow previous results to affect future parsers: '>>='.

   We will need one final primitive parser: the one that just reads a
   single character from the input, and returns it as its parse
   result. -}

char :: Parser Char
char = MkParser parseChar
  where parseChar ""       = []
        parseChar (c:rest) = [(c,rest)]

{- The 'char' parser allows us to read the input one character at a
   time. Combined with the other combinators, we can now build more
   complex parsers that check and manipulate the input. Here is a
   parser that reads in digits -- i.e., only those 'Char's that are
   decimal digits. We use the standard library functions 'isNumber'
   and 'digitToInt' to check that a 'Char' is a digit and to convert
   it, respectively. -}

digit :: Parser Int
digit =
  char >>= \c ->
  if isNumber c then
    pure (digitToInt c)
  else
    empty

{- We can now use 'digit' to parse (positive) numbers by asking for a
   list of digits and turning it into a number. Because we defined an
   'Alternative' instance for 'Parser', we get for free the following
   function from the Haskell standard library:

     some :: Alternative f => f a -> f [a]

   The definition of 'some p' means one or more copies of 'p'. It can
   be given a concise, if cryptic definition in terms of the
   Applicative and Alternative combinators:

     some p = ((:[]) <$> p) <|> ((:) <$> p <*> some p)

   Haskell sometimes has a reputation for unreadable use of infix
   operators and puncutation, and this definition is one of the worst
   culprits. A more perspicious way of defining this function is:

     some p = (\x -> [x]) <$> p
              <|>
              (\x xs -> x:xs) <$> p <*> some p

   which makes it clear that there is a choice between one copy of
   'p', which results in a singleton list (remember that the 'action'
   of a rule is written to the left of the '<$>'), and a copy of 'p'
   followed by 'some p'.

   There is also the function 'many', defined in the standard
   library', that performs zero or more copies.

   Using 'some' (which we don't have to define explicitly because we
   get it for free) and 'digit', we can define a parser that parses
   sequences of one or more digits: -}

digits :: Parser [Int]
digits = some digit

{- To parse natural numbers, we now use the 'Functor' instance for
   'Parser's to convert a list of digits to a number. The actual
   conversion is accomplished by a left fold over the list,
   multiplying by ten and adding to shift each digit to the right
   position: -}

number :: Parser Int
number = foldl (\acc d -> acc*10 + d) 0 <$> digits

{- Let's test this parser to see if it works:

       λ> runParser number "10"
       [10]
       λ> runParser number "0"
       [0]
       λ> runParser number "7986213"
       [7986213]
       λ> runParser number ""
       []
       λ> runParser number "abc"
       []

   EXERCISE: Rewrite 'string' in terms of 'char', 'many', and (>>=). -}

{------------------------------------------------------------}
{- Part 5. Parsing Expressions                              -}

{- Parsing numbers and strings is all very well, but Exercise 4 is going
   to be about parsing a programming language. Let's start small by
   writing a parser for our little language of numbers and addition: -}

data Expr
  = Number Int
  | Add    Expr Expr
  deriving Show

{- The final goal is to parse strings that look like:

       "2+(4+5)+1"

   into the corresponding value:

       Add (Number 2) (Add (Add (Number 4) (Number 5)) (Number 1))

   where we have resolved the ambiguity caused by a lack of
   parentheses by nesting to the right.

   Thinking about this as a grammar, our first attempt might be:

      E ::= n
          | E '+' E
          | '(' E ')'

   (ignoring direction of nesting for now). We can attempt to
   translate this directly into our 'Parser's as follows: -}

expr_v1 :: Parser Expr
expr_v1 =  (\n -> Number n)        <$> number
       <|> (\e1 _ e2 -> Add e1 e2) <$> expr_v1 <*> string "+" <*> expr_v1
       <|> (\_ e _ -> e)           <$> string "(" <*> expr_v1 <*> string ")"

{- This definition looks very elegant, and apart from the additional
   action parts to translate the parse results into 'Expr' trees, it
   looks very close to the grammar. Unforunately, it doesn't work:

       λ> runParser expr_v1 "2+(4+5)+1"
       *** Exception: stack overflow

   Even when parsing just a single number, we nearly get somewhere,
   but then it fails:

       λ> runParser expr_v1 "1"
       [Number 1*** Exception: stack overflow

   The problem is that the original grammar contains /left
   recursion/. The second case for 'E' immediately refers to 'E' again
   (reading left-to-right), without consuming any symbols. In terms of
   our definition of 'expr_v1' this amounts to the following sequence
   of decisions made:

   1. To parse an 'expr_v1', there are three choices.

   2. The first choice is a number, which succeeds, but returns
   leftovers.

   3. The second choice is to parse "E '+' E", so we call 'expr_v1' to
   parse an 'E', but this means we go back to (1) again, with the same
   input.

   Hence, we get stuck in a loop and eventually crash with a stack
   overflow exception.

   The fix to this problem is to make sure that there are no calls
   back to the same parser without making progress through the
   input. A crude way of ensuring this is to demand that all
   expressions are completely bracketed. So we will only accept input
   like:

       "(2+((4+5)+1))"

   We do this like so, asking that each '+' expression is wrapped in
   parentheses: -}


expr :: Parser Expr
expr =  Number <$> number
    <|>
        (\_ e1 _ e2 _ -> Add e1 e2) <$> string "(" <*> expr <*> string "+" <*> expr <*> string ")"

{- Now we have:

       λ> runParser expr "(2+((4+5)+1))"
       [Add (Number 2) (Add (Add (Number 4) (Number 5)) (Number 1))]

   as required.

   In the next lecture, we will talk about how to write a parser for
   expressions that does not require so many brackets. -}
