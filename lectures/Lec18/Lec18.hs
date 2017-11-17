module Lec17 where

-- We will define our own versions of getLine, putStr, and putStrLn
-- below, so we hide the ones from the base library
import Prelude hiding (getLine, putStr, putStrLn)

{- 1. Compiling and running programs                       -}

{- So far, we have been running functions in the ghci REPL. Today, we
   will run programs from the command line, so we need to actually
   compile them.
-}

{- 1.1 The main function -}

{- The compiler will go looking for a function 'main' of type IO () --
   we will come back to this IO type soon -- in a module called Main
   and start running the program from there. Since the current module
   is not called Main, we introduced a new file Main.hs for the main
   function. There, we imported this module, and for this to work, we
   made sure that the file name matched the module name.
-}

-- The main function in Main.hs is using this definition by importing
-- this file
message :: String
message = "Hello world!"

{- 1.1 Compiling and running -}

{- We compiled the program by running 'ghc --make Main.hs'

      $ ghc --make Main.hs
      [1 of 2] Compiling Lec17            ( Lec17.hs, Lec17.o )
      [2 of 2] Compiling Main             ( Main.hs, Main.o )
      Linking Main ...

   and produced an executable 'Main'. Running it produced some output:

      $ ./Main
      Hello World!
-}

{- 2. Input and output                                     -}

{- We think if the type IO a as the type of "effectful actions that when
   run will possibly do some input or output, and produce a value of
   type a". This makes it possible to separate "pure" values from
   "impure" computations.

   We already saw that main needs to have type IO (). So main is a
   program that does some input and output, and then produces a dummy
   unit value -- we are only interested in main for its input/output
   side effects.
-}

{- 2.1 Keeping pure values and impure computations separate-}

{- Many programming languages confuse e.g. IO String and
   String. Assuming that we have an action

     getLine :: IO String

   that gets a line of input from the user, what should the result of

     getLine == getLine

   be? True or false? Probably false, because the user might very well
   give different inputs. But then we have lost the rather nice
   property that everything is equal to itself... However, if we do
   not confuse String and IO String, the problem disappears: if we
   type the above into ghci, we get

     No instance for (Eq (IO String)) arising from a use of ‘==’
     In the expression: getLine == getLine

   So by keeping IO String separate from String, we can keep the nice
   properties of the latter, which makes reasoning about our code much
   easier.
-}

{- 2.2 Primitive IO operations -}

{- We are given some primitive operations for working with IO, such as

   Getting and outputting a character:

     getChar  :: IO Char
     putChar  :: Char -> IO ()

   Getting and outputting a line:

     getLine   :: IO String
     putStr   :: String -> IO ()
     putStrLn :: String -> IO ()

   The function putStrLn also prints a newline at the end.
-}

{- 2.3 How do we combine IO actions? It's a monad! -}

{- We can use the usual monad machinery such as >>= and >> for combining
   IO actions. Using do-notation usually results in quite readable
   programs that look a lot like imperative code:
-}

greeter :: IO ()
greeter = do
  putStrLn "Who do you want to greet?"
  name <- getLine
  putStrLn $ "Hello " ++ name ++ "!"

-- Alternatively, we could have written

greeter2 :: IO ()
greeter2 = putStrLn "Who do you want to greet?" >>
           getLine >>= \ name -> putStrLn $ "Hello " ++ name ++ "!"

-- which is arguably less readable. If we didn't insist on the final
-- exclamation mark, it would have looked nicer:

greeter2' :: IO ()
greeter2' = putStrLn "Who do you want to greet?" >>
            getLine >>=  putStrLn .  ("Hello " ++)

{- After binding a value such as name, we can use it like any other
 value to e.g. determine the control flow:
-}

boredGreeter :: IO ()
boredGreeter = do
  putStrLn "Who do you want to greet?"
  name <- getLine
  if name == "World" then
    putStrLn "How predictable..."
  else
    putStrLn $ "Hello " ++ name ++ "!"

{- 2.5 Derived operations -}

{- Not all primitive operations are needed; some can be defined in terms
   of others. Of course, this is not how it is done in the standard
   library, but this is a good exercise for us:
-}

getLine :: IO String
getLine = do
  x <- getChar
  if x == '\n' then
    return []
  else
    do xs <- getLine
       return (x:xs)

-- Note the use of 'return' to turn pure values such as [] and (x:xs)
-- into IO actions, like in any other monad

putStr :: String -> IO ()
putStr []     = return ()
putStr (x:xs) = do
  putChar x
  putStr xs

putStrLn :: String -> IO ()
putStrLn msg = do
  putStr msg
  putChar '\n'

{- 2.6 Is Haskell a purely functional language? -}

{- We briefly pondered if the existence of IO means that Haskell is not
   a purely functional language. One definition of a purely functional
   language is a functional language in which calling the same
   function multiple times with the same arguments will always produce
   the same results. So in this sense, Haskell is pure: calling
   getLine multiple times will always result in the same IO action: an
   action which the run-time system will interpret as calling out to
   the operating system routine that is responsible for getting input
   from the keyboard. We can think of the the program as a pure
   program that produces *values* of type IO String; the magic happens
   when the main function is actually executed.
-}

{- 2.7 Other useful IO functions -}

{- Many other side-effects such as reading or writing to disk ultimately
   boil down to doing some input/output, so there are many functions
   that end up in the IO monad, e.g.

     readFile   :: FilePath -> IO String
     writeFile  :: FilePath -> String -> IO ()
     appendFile :: FilePath -> String -> IO ()

   where FilePath is a predefined type alias

     type FilePath = String

   Related to reading from and writing to files, remember that there
   are typeclasses for converting to and from strings, with functions

     read :: Read a => String -> a
     show :: Show a => a -> String

   Note that these functions have nothing to do with IO!

   Finally, another two useful IO functions are defined in
   System.Environment:

     getArgs     :: IO [String]
     getProgName :: IO String

   The action 'getArgs' returns the list of command line arguments to
   the program, and 'getProgName' returns the name of the program as
   it was invoked.
-}

-- We saw an example of using these in NumberLines.hs (we wrote it in
-- a separate file so that it could have a main function in a main
-- module)

{- Compiling and running NumberLines on its own source code

     ghc --make NumberLines.hs
     ./NumberLines NumberLines.hs out.txt

   produces the following in out.txt:

     1: module Main where
     2: 
     3: import System.Environment
     4: 
     5: -- add line numbers to the beginning of each line
     6: 
     7: main = do
     8:   xs <- getArgs
     9:   case xs of
     10:     [inFile, outFile] ->
     11:       do file <- lines <$> readFile inFile
     12:          let out = zipWith (\ n xs -> (show n) ++ ": " ++ xs) [1..] file
     13:          writeFile outFile (unlines out)
     14:     _ -> do putStrLn "Wrong number of arguments"
     15:             prog <- getProgName
     16:             putStrLn $ "Usage: " ++ prog ++ " inFile outFile"
     17: 
-}

{- 2.8 Interacting with libraries -}

{- Finally, because talking to external bindings and libraries often
   involves some IO, the top-level interfaces to these libraries are
   often wrapped in the IO monad as well. We saw an example of using
   the Gloss library (http://gloss.ouroborus.net/ ) to draw a fractal
   tree on the screen, and how we could play around with the code to
   add more branches or more detail to the tree.

   The source code can be found in Tree.hs.
-}
