module Ex4 where

import Control.Applicative hiding (many)
import Control.Monad
import Data.Char
import Data.Foldable
import Data.List

{----------------------------------------------------------------------}
{- CS316 (2017/18) EXERCISE 4                                         -}
{----------------------------------------------------------------------}

{- Submit by committing to GitLab at or before 2pm on Monday 27th
   November.  There will be a test on this exercise in the lab on that
   date.

   Your combined score from the submission and the test will be worth
   35% of the overall marks for the class (so one mark, below is worth
   half a percent).

   The test will consists of further requirements issued as updates to
   this file, and you will need to make changes in response to the new
   requirements, then commit a new version of the file by the end of
   the lab session. -}

{- 4.1 Identify yourself. Encode your name instead of Harry's between the
   quotation marks. Your file might get separated from your repository,
   so we'll need this info to give you your mark. -}

myName :: String
myName = map pred "Ibssz!Qbmnfs"

{- 1 MARK -}

{----------------------------------------------------------------------}
{- GHOUL : Global Higher-order Untyped Language                       -}
{----------------------------------------------------------------------}

{- INTRODUCTION TO GHOUL

   This exercise is about building an interpreters for a functional
   language called "GHOUL". It will bring together all of the concepts
   you have learned during this course.

   Here is an example GHOUL program (the backslashes '\' at the start
   and end of the lines are how Haskell allows multi-line strings): -}

plusProgram :: String
plusProgram =
  "plus(Z,y) = y;\
  \plus(S(x),y) = S(plus(x,y));\
  \main() = plus(S(S(Z)),S(S(Z)));"

{- Execution of GHOUL programs works by starting from the main()
   function and then matching each function application against the
   patterns defined for that function to get an expression to replace
   that application with. This continues until there are no more
   expressions to replace. Data is built from constructors
   (identifiers that start with capital letters) applied to other
   constructors.

   For our example program, we have:

         main()
      => plus(S(S(Z)),S(S(Z)))
      => S(plus(S(Z),S(S(Z))))
      => S(S(plus(Z,S(S(Z)))))
      => S(S(S(S(Z))))

   So "two plus two is four".

   GHOUL is similar to Haskell, except that there are no types and
   there are no lambda expressions (\x -> ...).

   In this exercise, you will build a GHOUL interpreter, and extend it
   with additional features.

   In general, all interpreters perform the following steps:

     1. Parse the input -- take a representation of a program as a
        list of characters and turn it into abstract syntax. You will
        do this using parser combinators.

     2. Post-process and check the input, often called "Elaboration"
        or "Type Checking". In Haskell this is a complex stage
        involving desugaring, type inference, and typeclass
        resolution. For GHOUL, you will write some code to perform
        some well-formedness checks on the program.

     3. Execute the program. In Lecture 11, we showed you evaluators
        for several very simple languages. GHOUL has three features that
        make evaluation more complex:

          a. Variables
          b. Named function definitions.
          c. Pattern matching.

   These steps are summed up by the function that parses, checks, and
   executes GHOUL programs: -}

runGHOUL :: String -> Either ErrorMessage (Either ErrorMessage Val)
runGHOUL text = do
  prog <- parseAllInput equations text
  checkProgram prog
  return (evalProgram prog)

{- 'runGHOUL' accepts a String containing a GHOUL program, parses it,
   elaborates it, checks it, and executes it. If any of these steps
   fail, an error message is returned. Otherwise, the result of
   evaluating the program is returned. Error messages are just
   strings, but we have defined a type synonym to make the types more
   readable: -}

type ErrorMessage = String

{- Of course, 'runGHOUL' doesn't work yet -- you will need to fill in
   the details below.

   When you've written at least the parser and evaluator parts, you
   should be able to use 'runGHOUL' to run a GHOUL program:

         λ> runGHOUL plusProgram
         Right (Right (VC "S" [VC "S" [VC "S" [VC "S" [VC "Z" []]]]]))

   This exercise is structured so that you can implement a basic GHOUL
   interpreter first, and then go back to extend it with additional
   features for extra marks. As with the previous exercises, roughly a
   third of the marks will only be available during the class test on
   Monday 27th November. -}

{----------------------------------------------------------------------}
{- Part 0 : ABSTRACT SYNTAX                                           -}
{----------------------------------------------------------------------}

{- Before we can write an interpreter for GHOUL programs, we need to
   describe what the syntax of GHOUL programs is. Generalising from
   the example above, a GHOUL program is:

     - a list of equations, such as:

           plus(Z,y)    = y;
           plus(S(x),y) = S(plus(x,y));
           main() = plus(S(S(Z)),S(S(Z)));

       where:

     - an equation is a name, a list of patterns, an expression and a
       semicolon, such as:

           plus(Z,y)    = y;

       where:

     - a pattern is a variable name or a constructor name optionally followed by a 
       list of patterns in parentheses, such as:

                 Z
            or   S(Z)
            or   x
            or   Cons(x,y)

       and

     - an expression is a variable use, or an application of a named
       function to expressions, or an application of a constructor
       name to expressions, such as:

                 S(plus(x,y))
            or   plus(x,y)
            or   Z
            or   x

   Also, there must be a function definition named 'main' that takes
   no arguments.

   Following this description, we represent GHOUL programs as values
   of the type 'Program', where a 'Program' is a list of equations. -}

type Program =
  [Equation]

{- As we mentioned above, equations consist of:

    1. a name (e.g., "plus", "main")

    2. a list of patterns (patterns are defined below)

    3. an expression for the right hand side (expressions are defined
       below).

   We write a type for representing equations like so: -}

data Equation = MkEqn String [Pat] Exp
  deriving (Show, Eq)

{- A pattern is either a variable (PV), or a constructor name and a list
   of patterns (PC). This is similar to patterns in Haskell, except
   that GHOUL does not have a "catch all" pattern '_' -}

data Pat
  = PV String
  | PC String [Pat]
  deriving (Show, Eq)

{- An expression is either a variable (EV), an application of a named
   function (EA) or a an application of a constructor (EC). -}

data Exp
  = EV String
  | EA String [Exp]
  | EC String [Exp]
  deriving (Show, Eq)

{- Here is an example 'Program', representing the example program we saw
   above.

   It is worth spending time to understand the correspondence between
   the Haskell value below and the concrete syntax in the
   'plusProgram' variable defined above. -}

plusProgramAST :: Program
plusProgramAST =
  [ MkEqn "plus"
          [PC "Z" [], PV "y"]
          (EV "y")
  , MkEqn "plus"
          [PC "S" [PV "x"], PV "y"]
          (EC "S" [EA "plus" [EV"x", EV"y"]])
  , MkEqn "main"
          []
          (EA "plus" [EC "S" [EC "S" [EC "Z" []]],
                      EC "S" [EC "S" [EC "Z" []]]])
  ]

{- 4.2 Write a GHOUL program to concatenate lists.

   Write a GHOUL program to concatenate (append) two lists as a value
   of type 'Program'. Remember that to be a valid GHOUL program, you
   should also include a 'main' function definition. Use the example
   of the 'append' program written in Haskell given in Lecture 3 as a
   guide. -}

appendProgramAST :: Program
appendProgramAST =
  undefined

{- 3 MARKS -}

{----------------------------------------------------------------------}
{- Part 1 : PARSING                                                   -}
{----------------------------------------------------------------------}

{- Writing GHOUL programs as values of type 'Program' is all very well,
   but not very friendly. Instead, we will build a parser and
   elaborator that will take a String that represents a GHOUL program
   and turn it into a list of equations. A list of equations is not
   yet a program, so Part 2 will build an elaborator to convert lists
   of equations into proper 'Program's.

   You will build your parser using parser combinators, as introduced
   in Lecture 14. Unlike in Lecture 14, we will write parsers that
   produce error messages (see the 'Either' monad in Lecture 13),
   rather than just returning 'Nothing' on failure. -}

newtype Parser a = MkParser (String -> Either ErrorMessage (a, String))

{- Parsers are applied to 'String's by using the 'runParser' function,
   which returns the value parsed, and the left over input: -}

runParser :: Parser a -> String -> Either ErrorMessage (a,String)
runParser (MkParser p) input = p input

{- To parse a complete string all the way to the end, we use
   'parseAllInput', which checks that the end of the string has been
   reached using the 'eoi' parser. -}

parseAllInput :: Parser a -> String -> Either ErrorMessage a
parseAllInput p input =
  case runParser (p <* eoi) input of
    Right (a, _) -> Right a
    Left msg     -> Left ("Parse error: " ++ msg)

{- The rest of the parser combinator functions are at the end of this
   file. The main combinators that you will want to use to build your
   parsers are:

     - The Functor, Applicative, Monad, and Alternative interfaces
     - 'isChar' to parse given characters
     - 'string' is parse given strings
     - 'identifier' to parse identifiers: sequences of letters and numbers
       that must start with a letter.
     - 'spaces' to parse zero or more white space characters.
     - 'sepBy' to parse lists of things separated by something.

   To begin the GHOUL parser, you will first construct two parsers
   that recognise variable names and constructor names. We will use
   these later on as part of our complete pattern and expression
   parsers. -}

{- 4.3 Write a 'Parser' for 'variable names'.

   Follow the Haskell convention that a variable name is an identifier
   that starts with a lower case letter. Use the library function
   'isLower' to identify lower case letters. -}

varname :: Parser String
varname = undefined

{- Here are some tests that your 'varname' parser should pass:

     runParser varname "plus"  == Right ("plus", "")
     runParser varname "x"     == Right ("x", "")
     runParser varname "Plus"  == Left <error message>
     runParser varname ""      == Left <error message>
     runParser varname "plu s" == Right ("plu", " s")
     runParser varname "123"   == Left <error message>

  Note that the tests do not specify what error messages look
  like. That is up to you. -}

{- 1 MARK -}


{- 4.4 Write a 'Parser' for 'constructor names'.

   Follow the convention that a constructor name is an identifier that
   starts with an upper case letter. Use the library function
   'isUpper' to identify upper case letters. -}

constructorname :: Parser String
constructorname = undefined

{- Here are some tests that your 'constructorname' parser should pass:

     runParser constructorname "plus"  == Left <error message>
     runParser constructorname "x"     == Left <error message>
     runParser constructorname ""      == Left <error message>
     runParser constructorname "Plus"  == Right ("Plus", "")
     runParser constructorname "S"     == Right ("S", "")
     runParser constructorname "plu s" == Left <error message>
     runParser constructorname "123"   == Left <error message> -}

{- 1 MARK -}


{- 4.5 Parsing patterns.

   A pattern is either:

     - a variable name; or
     - a constructor name followed by a comma separated list of patterns, in
       parentheses; or
     - a constructor name.

   For example:

         Cons(Z,xs)

   Write a parser for patterns. -}

pat :: Parser Pat
pat = undefined

{- Here are some tests that your 'pat' parser should pass:

     runParser pat "x"      == Right (PV "x","")
     runParser pat "Z"      == Right (PC "Z" [],"")
     runParser pat "S(x)"   == Right (PC "S" [PV "x"],"")
     runParser pat "S(x,y)" == Right (PC "S" [PV "x",PV "y"],"")
     runParser pat ""       == Left <error message>
     runParser pat "S(x"    == Right (PC "S" [],"(x")
     runParser pat "S(x,"   == Right (PC "S" [],"(x,")
     runParser pat "x(x,y)" == Right (PV "x","(x,y)")

   Note the last two cases: they have only parsed part of the input,
   and returned the bit they couldn't parse. -}

{- 3 MARKS -}


{- 4.6 Parsing expressions

   An expression is either:

     - a variable name followed by a comma separated list of expressions, in
       parentheses, which is interpreted as a function call; or
     - a constructor name followed by a comma separated list of
       expressions, in parentheses; or
     - a variable name; or
     - a constructor name.

   For example:

        append(Cons(Z,Nil),xs)

   Write a parser for expressions. This will be very similar to the
   parser for patterns above, so it is worth fewer marks. -}

expr :: Parser Exp
expr = undefined

{- 2 MARKS -}


{- 4.7 Parsing Equations.

   The concrete syntax for individual equations looks like:

      plus(S(x),y) = S(plus(x,y));

   Points to note:

     1. An equation starts with a lower-cased identifier; then
     2. a list of patterns in parentheses, separated by commas
     3. an equals sign
     4. an expression
     5. a semicolon

   Using the 'pat' and 'expr' parsers you wrote above, write a
   'Parser' for equations. To be programmer friendly, you should allow
   spaces around the equals sign.

   Use the plusProg program from above for test cases. -}

equation :: Parser Equation
equation = undefined

{- 3 MARKS -}


{- 4.8 Parsing lists of Equations, aka Programs.

   The final stage of parsing is a parser for lists of equations,
   separated by zero or more spaces. You should also allow for spaces
   at the beginning and end of the input too. -}

equations :: Parser Program
equations = undefined

{- 2 MARKS -}

{--------------------------------------------------------------------}
{- 4.9 WILL APPEAR IN THE TEST                                      -}
{- 5 MARKS -}
{--------------------------------------------------------------------}


{--------------------------------------------------------------------}
{- Part 2 : CHECKING                                                -}
{--------------------------------------------------------------------}

{- The GHOUL interpreter that you will write below tries its best with
   any 'Program' that it is given, but there are some silly mistakes
   that programmers can make that can be relatively easily checked for
   before execution.

   (Note that this part is not needed to just run GHOUL programs --
    skip to Question 4.14 to complete the evaluator.)

   In this part, you will write a number of checks on GHOUL programs
   that check for mistakes like:

     - Not having a 'main' function, or the main function taking
       arguments. (Question 4.10 below).

     - Using variables on the right-hand side of an equation that are
       not mentioned in the pattern on the left-hand side (Question
       4.11, below). For example:

          plus(Z,x) = y;

       This equation will always fail during execution, because there
       is no value for 'y'.

     - Writing two equations with the same name, but different numbers
       of arguments (Question 4.12, below). For example:

          plus(Z) = Z;
          plus(S(x),y) = S(plus(x,y));

       This is a design decision -- we could allow GHOUL programs like
       this and think of there effectively being two 'plus' functions,
       one that takes one argument and another that takes
       two. However, we could also say that this would likely lead to
       confusing programs. If we want two plus functions like this,
       then they should have different names.

  The following function runs all the checks listed above. However,
  all of the functions that actually do the checking just immediately
  return successfully. It is your task to fill them in. -}

checkProgram :: Program -> Either ErrorMessage ()
checkProgram prog = do
  hasMainCheck prog
  scopeCheck prog
  arityCheck prog

{- In the functions below, you can use the 'abortWithMessage' function
   to report errors. See Lecture 13 for information on how to use the
   'Either' monad for error reporting. -}

abortWithMessage :: ErrorMessage -> Either ErrorMessage a
abortWithMessage msg = Left msg

{- 4.10 Checking for a main function

   Write a function that checks a 'Program' for an equation called
   'main' that has no arguments. If it doesn't, then you should return
   a useful error message. -}

hasMainCheck :: Program -> Either ErrorMessage ()
hasMainCheck prog =
  -- fill this in
  return ()

{- 1 MARK -}

{- 4.11 Scope checking

   Write a function that checks each equation is "well-scoped". This
   means that all the variables mentioned on the right-hand side (in
   the 'Exp') are mentioned on the left-hand side (in the
   patterns). You may find it helpful to write a function that checks
   individual equations first. -}

scopeCheck :: Program -> Either ErrorMessage ()
scopeCheck prog =
  -- fill this in
  return ()

{- 5 MARKS -}

{- 4.12 Arity Checking

   Write a function that checks that all the equations with the same
   name have the same number of patterns.

   You will find the library functions 'sort' and 'groupBy' helpful for
   grouping together all the equations with the same name. -}

arityCheck :: Program -> Either ErrorMessage ()
arityCheck prog =
  -- fill this in
  return ()

{- 4 MARKS -}

{----------------------------------------------------------------------}
{- 4.13 WILL APPEAR IN THE TEST                                       -}
{- 3 MARKS -}
{----------------------------------------------------------------------}

{----------------------------------------------------------------------}
{- Part 3 : EXECUTION                                                 -}
{----------------------------------------------------------------------}

{- Execution of GHOUL programs results in values, which are defined to
   be constructors applied to lists of values: -}

data Val = VC String [Val]
         deriving (Show, Eq)

{- For example, the expression S(Z) evaluates to the 'Val'ue:

      VC "S" [VC "Z" []]

   GHOUL programs have variables in them. To keep track of what each
   variable means by during execution, we use environments. An
   environment is a list containing pairs of names that are attached
   to values: -}

type Env = [(String, Val)]

{- For example, the empty environment (no variables have values) is the
   empty list:

        []

   An environment that assigns the GHOUL value 'Z' to 'x' and 'S(Z)'
   to 'y' is the list:

        [ ("x", VC "Z" [])
        , ("y", VC "S" [VC "Z" []])
        ]

    We look up the values assigned to variables in an environment
    using the 'lookup' function:

       > :t lookup
       lookup :: Eq a => a -> [(a, b)] -> Maybe b
       > lookup "x" [ ("x", VC "Z" []), ("y", VC "S" [VC "Z" []])]
       Just (VC "Z" [])
       > lookup "z" [ ("x", VC "Z" []), ("y", VC "S" [VC "Z" []])]
       Nothing
-}

{----------------------------------------------------------------------}
{- Part 3(a) : IMPLEMENTING PATTERN MATCHING                          -}
{----------------------------------------------------------------------}

{- Execution of a GHOUL program alternates between choosing which
   equation to apply, and then evaluating the right-hand side of that
   equation. Choosing which equation to apply is accomplished by
   pattern matching.

   The essence of pattern matching is to produce a mapping from
   variable names to values by matching a pattern against a compound
   value. To help with pattern matching, we use 'Matcher's: things
   that transform environments, but may fail: -}

newtype Matcher a = MkMatcher (Env -> Maybe (a, Env))

{- To test the matching functions that you will write below, use
   'runMatcher', which runs a matcher starting with an empty
   environment and returns the resulting value and environment. -}

runMatcher :: Matcher a -> Env -> Maybe (a, Env)
runMatcher (MkMatcher m) = m

{- 'Matcher' is a Functor, Applicative and Alternative, which will help
   you write the GHOUL-specific matching implementations
   below. 'Matcher' can also be given a 'Monad' structure, but it is
   possible to write a matcher without using that fact. -}

instance Functor Matcher where
  fmap f (MkMatcher m) =
    MkMatcher (fmap (\(a, env) -> (f a, env)) . m)

instance Applicative Matcher where
  pure a =
    MkMatcher (\env -> pure (a, env))

  mf <*> ma =
    MkMatcher (\env -> do
                  (f, env')  <- runMatcher mf env
                  (a, env'') <- runMatcher ma env'
                  return (f a, env''))

instance Monad Matcher where
  return = pure

  m >>= f =
    MkMatcher (\env -> do
                  (a, env') <- runMatcher m env
                  runMatcher (f a) env')

instance Alternative Matcher where
  empty =
    MkMatcher (\env -> empty)

  m1 <|> m2 =
    MkMatcher (\env -> runMatcher m1 env <|> runMatcher m2 env)

{- The basic operation that make 'Matcher's special is the ability to
   bind variables to values. This is the basic step in pattern
   matching. 'bindVar' takes a variable name and a value, and returns
   the 'Matcher' that performs the action of binding that variable to
   that value. Note that if the named variable already has a binding,
   then 'bindVar x v' signals failure by returning 'Nothing'. -}

bindVar :: String -> Val -> Matcher ()
bindVar x v =
  MkMatcher (\env -> case lookup x env of
                       Nothing -> Just ((), (x,v):env)
                       Just _  -> Nothing)

{- Let's see how this works using 'runMatcher'. If we bind "x" to the
   value 'VC "Z" []' starting with the empty environment, we get back
   an environment extended with the new assignment.

     > runMatcher (bindVar "x" (VC "Z" [])) []
     Just ((),[("x",VC "Z" [])])

   However, if the environment already has a assignment for "x", then
   we get back 'Nothing':

     > runMatcher (bindVar "x" (VC "Z" [])) [("x", VC "Nil" [])]
     Nothing
-}

{- We will be matching lists of patterns against lists of values. The
   obvious way to do this is to use the 'zip' function from the
   Haskell library. However, if the input lists are different lengths,
   then the standard 'zip' ignores all the extra elements in the
   longer list. So we define our own 'zipChecked' function that uses a
   'Maybe' to signal when the lists are different lengths. -}

zipChecked :: [a] -> [b] -> Maybe [(a,b)]
zipChecked xs ys = go xs ys []
  where go []     []     zs = Just (reverse zs)
        go (x:xs) (y:ys) zs = go xs ys ((x,y):zs)
        go _      _      _  = Nothing

{- 4.14 Matching patterns and lists of patterns.

        Write the functions 'matchPat' and 'matchPats'.

   'matchPat' takes a pair of a pattern and a value and returns a
   Matcher. These ought to implement pattern matching:

     - matching a variable against any value binds the variable to the
       value.

     - matching a constructor pattern against a value checks that the
       value has a constructor with the same name, and that the
       sub-patterns match all of the sub-values.

   'matchPats' should take a list of patterns and a list of values,
   and return a Matcher generated by matching all the pairs. If the
   two lists have different lengths, then matching ought to fail. Use
   the 'zipChecked' function we gave you above. -}

matchPat :: (Pat, Val) -> Matcher ()
matchPat = undefined

matchPats :: [Pat] -> [Val] -> Matcher ()
matchPats = undefined

{- 3 MARKS -}

{- 4.15 Finding Equations by Pattern Matching

   Write a function that, given a name and a list of values, searches
   a 'Program' for the first equation that matches. That is, the names
   should match, and the patterns of the equation should match the
   list of values. On success, you should return the expression
   associated with that pattern.

   One way to write this function is to use the 'Alternative'
   typeclass functions 'empty' and '<|>' to represent failure to find
   an equation and ordered choice, respectively. -}

findMatchingEquation :: String -> [Val] -> Program -> Matcher Exp
findMatchingEquation = undefined

{- 4 MARKS -}

{----------------------------------------------------------------------}
{- 4.16 WILL APPEAR IN THE TEST                                       -}
{- 2 MARKS -}
{----------------------------------------------------------------------}

{----------------------------------------------------------------------}
{- 4.17 WILL APPEAR IN THE TEST                                       -}
{- 3 MARKS -}
{----------------------------------------------------------------------}

{----------------------------------------------------------------------}
{- Part 3(b): EVALUATION OF EXPRESSIONS                               -}
{----------------------------------------------------------------------}

{- Evaluation of expressions in the context of some program and some
   environment is modelled using the 'Eval' data type. The 'Eval' type
   offers four services as well as being a 'Monad':

     a) The ability to look at the current program ('currentProgram')

     b) The ability to look at the current environment
        ('currentEnvironment')

     c) The ability to report failed execution ('abortEval')

     d) The ability to do pattern matching to change the current
        environment ('afterMatch') -}

newtype Eval a =
  MkEval (Program -> Env -> Either ErrorMessage a)

{- To 'run' some evaluation, use the 'runEval' function that runs an
   evaluation with a given program and environment: -}

runEval :: Eval a -> Program -> Env -> Either ErrorMessage a
runEval (MkEval e) program env =
  e program env

{- 'Eval' supports the Monad operations 'return' and '>>=', which should
   not be surprising since it is the combination of the 'Reader' monad
   (Lecture 16), and the 'Maybe' monad (Lecture 13). As a consequence
   it also supports the 'Functor' and 'Applicative' interfaces: -}

instance Monad Eval where
  return x = MkEval (\prg env -> return x)

  e >>= k = MkEval (\prg env -> do a <- runEval e prg env
                                   runEval (k a) prg env)

instance Functor Eval where
  fmap f ea = do a <- ea; return (f a)

instance Applicative Eval where
  pure = return
  ef <*> ea = do f <- ef; a <- ea; return (f a)

{- The three basic operations supported by the 'Eval' monad are the ones
   that abort evaluation with an error message ('abortEval'), access
   the current environment ('currentEnvironment'), and access the
   program being executed ('currentProgram'). You will need to use
   these below in order to implement the parts of evaluation of
   expressions that require looking up names in the environment or the
   global program. -}

abortEval :: ErrorMessage -> Eval a
abortEval msg = MkEval (\prg env -> abortWithMessage msg)

currentEnvironment :: Eval Env
currentEnvironment = MkEval (\prg env -> return env)

currentProgram :: Eval Program
currentProgram = MkEval (\prg env -> return prg)

{- The 'afterMatch' function links pattern matching and
   evaluation. Given a function:

      f : a -> Eval b

   that takes 'a's and produces evaluation results of type 'b', and a
   'Matcher' that returns 'a's:

      m : Matcher a

   then 'afterMatch f m', runs the matcher 'm' to generate a new
   environment and a value 'a' and then runs the evaluation 'f a' in
   the environment generated by the pattern match. The program
   definitions are then passed through, because they are global.

   Some examples. All of these use 'currentEnvironment' as the
   evaluation to run after the pattern match, so the result we will
   see in each case is the environment that the evaluation run after
   the pattern match is executed in. Explanations follow each example.

       λ> let v = VC "Z" []
       λ> runEval ((\_ -> currentEnvironment) `afterMatch` (pure ())) [] [("x", v)]
       Right []

   So the 'null' pattern match (represented by 'pure ()') means that
   after the match we get the empty environment, even though we
   started with the environment '[("x", v)]'.

       λ> runEval ((\_ -> currentEnvironment) `afterMatch` (bindVar "y" v)) [] [("x", v)] 
       Right [("y",VC "Z" [])]

   A 'matcher' computation that binds a variable, 'bindVar "y" v',
   results in an environment with that variable bound, and ignores any
   bindings in the outer environment.

       λ> runEval ((\_ -> currentEnvironment) `afterMatch` empty) [] [("x", v)] 
       Left "Match failure"

   Evaluation after a failing pattern match, 'empty', results in a
   failing evalution. -}

afterMatch :: (a -> Eval b) -> Matcher a -> Eval b
afterMatch f (MkMatcher m) =
  MkEval (\prg _ -> case m [] of
                      Nothing       -> abortWithMessage "Match failure"
                      Just (a, env) -> runEval (f a) prg env)

{- 4.17 Evaluating programs

   The final step in implementing a GHOUL interpreter is in the
   implementation of 'eval' for expressions. Implement this function,
   following this informal specification:

     1) constructor applications 'EC' evaluate all their arguments to
     values, and return a 'VC' value with the given constructor name
     applied to all the values.

     2) variables 'EV' are evaluated by looking up their value in the
     current environment.

     3) function applications 'EA' are evaluated by looking up the
     definition associated with the function name, evaluating all the
     arguments to values, and the applying that definition to those
     values. -}

eval :: Exp -> Eval Val
eval = undefined

evalProgram :: Program -> Either ErrorMessage Val
evalProgram prog =
  runEval                 -- run an evaluation
    (eval (EA "main" [])) -- of the expression that calls the 'main' function
    prog                  -- in the given program
    []                    -- and the empty environment

{- 4 MARKS -}


{----------------------------------------------------------------------}
{- Part 4: EXTRA FEATURES                                             -}
{----------------------------------------------------------------------}

{----------------------------------------------------------------------}
{- 4.19 Logging Effects

   Alter the GHOUL language so that there is a logging facility that
   records GHOUL values in a log as execution proceeds. You can use
   the 'show' function for the 'Val' datatype to turn GHOUL values
   into strings:

      show :: Val -> String

   You will need to:

    - Add a special 'print' operation to the GHOUL syntax that allows
      for printing to happen. You decide what the syntax should look
      like, and how it ought to behave (assuming that it does actually
      log values).

    - Alter the 'eval' and 'evalProgram' functions so that they return
      '([String], Either ErrorMessage Val)', where the first part of
      the pair is the logged messages, and the second is the returned
      value (or Nothing for failure). You will need to alter the
      'Eval' monad to record logging information.

    - You will need to change the definition of 'runGhoul' so that its
      type is:

         runGHOUL :: String -> Either ErrorMessage ([String], Either ErrorMessage Val)


   You may find the LogAndFail example in Lecture 16 useful.

   So that we know what you've done to add logging, please give a
   short list of the changes you've made below. -}

{- 10 MARKS -}



{----------------------------------------------------------------------}
{- 4.20 WILL APPEAR IN THE TEST                                       -}
{- 10 MARKS -}
{----------------------------------------------------------------------}



{--------------------------------------------------------------------}
{- APPENDIX : PARSER COMBINATORS                                    -}
{--------------------------------------------------------------------}

{- Here is the code for the parser combinators you should use to
   implement your GHOUL parser. You may want to consult this code to
   help you write your parser, but do not alter it. -}

instance Functor Parser where
  fmap f (MkParser p) =
    MkParser (fmap (fmap (\(a,s) -> (f a,s))) p)

instance Applicative Parser where
  pure x = MkParser (\s -> Right (x,s))

  MkParser pf <*> MkParser pa =
    MkParser (\s -> case pf s of
                      Left msg ->
                        Left msg
                      Right (f, s1) ->
                        case pa s1 of
                          Left msg ->
                            Left msg
                          Right (a, s2) ->
                            Right (f a, s2))

instance Monad Parser where
  MkParser p >>= k =
    MkParser (\s -> case p s of
                      Left msg -> Left msg
                      Right (a, s1) ->
                        let MkParser p2 = k a in
                          p2 s1)

instance Alternative Parser where
  empty =
    MkParser (\s -> Left "<empty>")

  MkParser p1 <|> MkParser p2 =
    MkParser (\s -> case p1 s of
                      Left _       -> p2 s
                      Right (a, s) -> Right (a,s))

eoi :: Parser ()
eoi = MkParser (\s -> case s of
                        "" -> Right ((), "")
                        s  -> Left ("expecting end of input; got " ++ show s))

parseFail :: String -> Parser a
parseFail msg = MkParser (\s -> Left msg)

char :: Parser Char
char = MkParser p
  where p []     = Left "expecting a character, but end of input was found"
        p (c:cs) = Right (c, cs)

isChar :: Char -> Parser ()
isChar expected = do
  seen <- char
  if expected == seen then
    return ()
  else
    parseFail ("Expecting " ++ show expected ++ ", got " ++ show seen)

satisfies :: String -> (Char -> Bool) -> Parser Char
satisfies p_description p = do
  c <- char
  if p c then return c else parseFail ("Expecting " ++ p_description ++ ", got " ++ show c)

string :: String -> Parser ()
string = mapM_ isChar

digit :: Parser Int
digit = do
  c <- char
  if isNumber c then
    return (digitToInt c)
  else
    parseFail "Expecting a digit"

number :: Parser Int
number = foldl (\l r -> l*10+r) 0 <$> some digit

space :: Parser ()
space = () <$ satisfies "a space character" isSpace

spaces :: Parser ()
spaces = () <$ many space

identifier :: Parser String
identifier = (:) <$> satisfies "alphabetic character" isAlpha
                 <*> many (satisfies "alphanumeric character" isAlphaNum)

sepBy :: Parser () -> Parser a -> Parser [a]
sepBy sep p = (:) <$> p <*> many (sep *> p) <|> pure []

many :: Parser a -> Parser [a]
many p = (:) <$> p <*> many p <|> pure []
