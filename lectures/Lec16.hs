module Lec16 where

{-    LECTURE 16 : READING, WRITING, AND THE STATE -}


{-    PART 1 : READING, with THE READER MONAD -}


-- FIXME: introduce in terms of passing around an argument

data Reader r a = MkR (r -> a)

data Environment = Opt { username    :: String
                       , isSuperuser :: Bool
                       }
  deriving Show

ask :: Reader a a
ask = MkR (\x -> x)


{-    PART 2 : PRINTING, or THE WRITER MONAD -}

-- FIXME: refer back to 

data Printing a = MkPr String a

instance Show a => Show (Printing a) where
  show (MkPr msg a) = msg ++ "\n=====\n\n" ++ (show a)


fib :: Integer -> Printing Integer
fib n = undefined


{-   PART 3 : STATE -}

-- FIXME: introduce in terms of threading a value through.

newtype State s a = MkState (s -> (a,s))
