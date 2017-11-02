module Ex3 where

import Text.Read

{----------------------------------------------------------------------}
{- CS316 (2017/18) EXERCISE 3                                         -}
{----------------------------------------------------------------------}

-- Submit by committing to GitLab at or before 2pm on Monday 6th
-- November.  There will be a test on this exercise in the lab on that
-- date.
--
-- Your combined score from the submission and the test will be worth
-- 30% of the overall marks for the class (so one mark, below is worth
-- half a percent).
--
-- The test will consists of further requirements issued as updates to
-- this file, and you will need to make changes in response to the new
-- requirements, then commit a new version of the file by the end of
-- the lab session.

{----------------------------------------------------------------------}
{- HIGHER-ORDER PROGRAMMING                                           -}
{----------------------------------------------------------------------}

{- 3.1 Identify yourself. Encode your name instead of Harry's between the
   quotation marks. Your file might get separated from your repository,
   so we'll need this info to give you your mark. -}

myName :: String
myName = map pred "Ibssz!Qbmnfs"

{- 1 mark -}

{----------------------------------------------------------------------}
{- STRUCTURAL RECURSION ON TREES AND LISTS                            -}
{----------------------------------------------------------------------}

{- In the last exercise, we asked you to write functions by recursion on
   lists and trees. These functions only did one thing. For example,
   'concLists' concatenated lists, 'mirror' mirrored trees.

   In this exercise, you will be using and writing so-called "higher
   order" functions that can do many different things, depending on
   the functions that are passed to them as parameters. -}

{- 3.2. Write filter's evil twin that retains the elements of a list
   that fail the test rather than those that pass. Write your function
   using 'filter'. -}

discard :: (a -> Bool) -> [a] -> [a]
discard p xs = undefined

{- 1 mark -}

{- There are two very general ways of iterating over the elements of a
   list, combining all the elements one by one. We can either go
   'right to left' or 'left to right'. We call the first one
   'iterRight' and the second 'iterLeft', and they are defined as
   follows. Many functions on lists can be defined in terms of these
   functions, as you will see below. -}

iterRight :: (a -> b -> b) -> b -> [a] -> b
iterRight f z []     = z
iterRight f z (x:xs) = f x (iterRight f z xs)

iterLeft :: (b -> a -> b) -> b -> [a] -> b
iterLeft f acc []     = acc
iterLeft f acc (x:xs) = iterLeft f (f acc x) xs

{- These functions are sometimes called 'foldr' and 'foldl'. We use
   these names here to avoid confusion with the general 'fold'
   functions that will be introduced in Lecture 10. -}

{- 3.3 The 'sum' function computes the sum of (i.e. adds together) a
   list of integers. For example:

     sum [1,2,3] = 6

   Define this function using 'iterRight'. Do not use explicit
   recursion -- if you write 'sumFromIterRight' on the right hand
   side of the equals sign here, you are doing it wrong. -}
sumFromIterRight :: [Int] -> Int
sumFromIterRight = undefined

{- Here is a QuickCheck property that you can use to test this function
   against the built-in 'sum' function: -}

sumFromIterRight_eq_sum :: [Int] -> Bool
sumFromIterRight_eq_sum xs = sumFromIterRight xs == sum xs

{- 1 mark -}

{----------------------------------------------------------------------}
{- 3.4 WILL BE REVEALED IN THE TEST                                   -}
{- 2 marks                                                            -}
{----------------------------------------------------------------------}

{- 3.5 The reverse function reverses a list. For example:

     reverse [1,2,3] = [3,2,1]

   Define a function that does the same job, but written in terms of
   'iterLeft'. Again, if you write 'reverseFromIterLeft' on the right
   hand side of the equals sign here, you are doing it wrong. -}
reverseFromIterLeft :: [a] -> [a]
reverseFromIterLeft = undefined

{- 2 marks -}

{- 3.6 It is possible to define iterLeft from iterRight, but it is easy
   to get it wrong. Here are two candidate implementations: -}

iterLeftFromIterRight_1 :: (b -> a -> b) -> b -> [a] -> b
iterLeftFromIterRight_1 f n l = iterRight (flip f) n l

iterLeftFromIterRight_2 :: (b -> a -> b) -> b -> [a] -> b
iterLeftFromIterRight_2 f n l = iterRight (flip f) n (reverse l)

{- Which one is correct? Write a QuickCheck property that "proves" your
   answer is the right one. The property should compare the output of
   a given candidate with the output of the 'iterRight' function, and
   succeed for the correct implementation and fail for the incorrect
   one.

   You should have:

      quickCheck (iterLeftFromIterRight_prop iterLeftFromIterRight_X)
           Failed!, if X is the wrong implementation

   and

      quickCheck (iterLeftFromIterRight_prop iterLeftFromIterRight_X)
           +++ OK, if X is the right implementation
-}

type TestingType = ()  -- replace this with a type to test with

iterLeftFromIterRight_prop ::
  ((TestingType -> TestingType -> TestingType) -> TestingType -> [TestingType] -> TestingType)
  -> TestingType -> [TestingType] -> Bool
iterLeftFromIterRight_prop candidate n l = undefined

{- After you've worked it out, fill in this:

        <REPLACE THIS> is the correct implementation
-}

{- 2 marks -}

{- 'iterLeft' and 'iterRight' allow iteration through a list of
   elements, but don't provide access to the list being iterated
   over. The 'recList' function, provided below, does allow access to
   the underlying list. See how the type of the function argument
   includes an extra '[a]' compared to the corresponding part in the
   type of 'iterRight'. -}

recList :: (a -> ([a], b) -> b) -> b -> [a] -> b
recList f z []     = z
recList f z (x:xs) = f x (xs, recList f z xs)

{- 3.7 Define the insertion into an ordered list function, as seen in
   Lecture 5, but this time using 'recList' instead of explicit
   recursion. -}

insert :: Ord a => a -> [a] -> [a]
insert = undefined

{- Hint: you could use QuickCheck to compare your function to the one in
   Lecture 5 (after renaming the old one). -}

{- 1 mark -}

{- 3.8 Define 'iterRight' from 'recList', without using explicit
   recursion. -}
iterRightFromRecList :: (a -> b -> b) -> b -> [a] -> b
iterRightFromRecList = undefined

{- 1 mark -}

{- For 3.8, it may be helpful to know that you can write lambda
   functions that take pairs as arguments using pattern matching
   notation. For example:

    \(a,b) -> a
-}

{----------------------------------------------------------------------}
{- 3.9 WILL BE REVEALED IN THE TEST                                   -}
{- 3 marks -}
{----------------------------------------------------------------------}

{- Here is the Tree type again. Trees are built from 'Leaf's and
   'Node's, and each node has two children and a value of type
   'a'. Just as for lists, we can write higher-order functions that
   process trees. -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{- Here is the iterTree function that implements the recursion scheme
   for trees that we'll see in Lecture 9. You will use this function
   below to implement other functions on trees. -}

iterTree :: b -> (b -> a -> b -> b) -> Tree a -> b
iterTree l n Leaf = l
iterTree l n (Node ltree a rtree) =
  n (iterTree l n ltree) a (iterTree l n rtree)


{- 3.10 The 'mapTree' function applies a given function to every value
   stored within the tree, returning the new tree. For example

     mapTree (+1) (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf))
  ==
     Node (Node Leaf 2 Leaf) 3 (Node Leaf 4 Leaf)

  Define this function using iterTree. -}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree = undefined

{- 2 marks -}

{- 3.11 Define sumTree using iterTree. 'sumTree' adds up all the
   integers stored within a tree. For example:

     sumTree (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf))
  ==
     6

    As before, your 'sumTree' should be defined using 'iterTree'. If
    you write 'sumTree' on the right hand side of the equals, you are
    doing it wrong! -}
sumTree :: Tree Int -> Int
sumTree = undefined

{- 1 mark -}

{- 3.12 Define 'flatten' (as in Lecture 5) using 'iterTree'. -}

flatten :: Tree a -> [a]
flatten = undefined

{- Hint: you could use QuickCheck to compare your function to the one in
   Lecture 5 (after renaming the old one). -}

{- 1 mark -}

{- 3.13 'recTree' is similar to 'recList', but operates on trees. Copy
   the definition of 'iterTree' and extend it to have the type given
   below. -}

recTree :: b -> ((Tree a,b) -> a -> (Tree a,b) -> b) -> Tree a -> b
recTree = undefined

{- 2 marks -}

{----------------------------------------------------------------------}
{- 3.14 WILL BE REVEALED IN THE TEST                                  -}
{- 1 mark -}
{----------------------------------------------------------------------}

{- 3.15 Write 'insertFromRecTree' (assuming that the tree is ordered),
   using 'recTree'. This function should do the same job as the
   'insertBST' function defined in Lecture 5. -}
insertFromRecTree :: Ord a => a -> Tree a -> Tree a
insertFromRecTree = undefined

{- 2 marks -}

{- 3.16 We will now see that even though 'recTree' appears to be more
   powerful than 'iterTree', we can in fact define 'recTree' from
   'iterTree'. The trick is to pass extra information through the
   iteration and discard it at the end. We have given the start of the
   definition below. You have to:

    a) Alter the definition of Extra a b to correctly describe the
    extra information needed; and

    b) Implement 'leaf' and 'node' below. -}

type Extra a b = () -- REPLACE THIS

recTreeFromIterTree :: b -> ((Tree a,b) -> a -> (Tree a,b) -> b) -> Tree a -> b
recTreeFromIterTree n l t = snd (iterTree leaf node t)
  where
    -- leaf :: (Extra a b, b)
    leaf = undefined

    -- node :: (Extra a b, b) -> a -> (Extra a b, b) -> (Extra a b, b)
    node = undefined

{- 3 marks -}

{----------------------------------------------------------------------}
{- 3.17 WILL BE REVEALED IN THE TEST                                  -}
{- 2 marks -}
{----------------------------------------------------------------------}

{----------------------------------------------------------------------}
{- MODELLING COMMUNICATING PROCESSES                                  -}
{----------------------------------------------------------------------}

{- This exercise generalises the communicating processes from Exercise 2
   to allow processes that send and recieve data of any type, not just
   bits. These processes are also a kind of tree, except that now the
   number of choices of what to do next after an input is represented
   by a function. -}

{- We'll do the setup, then it'll be your turn. -}

data CP x a
  = End a -- marks the end of a process, returning a value of type a
  | Input (x -> CP x a) -- (Input k) inputs a value v of type x, and
                        -- chooses a continuation process (k v) based
                        -- on that value.
  | Output x (CP x a) -- (Output v k) outputs a value v of type x and
                      -- continues as the process k.
  | Abort -- signals that the process has aborted. This can happen if
          -- it gets unexpected input.

{- Just as in the 'Process' type in Exercise 2, the data in this type
   are descriptions of processes. We will see different ways of
   interpreting them below. -}

{- Let's have some example processes. First, the notGate example from
   Exercise 2, rewritten to be a member of the more general CP type: -}

notGate :: CP Bool ()
notGate = Input (\b -> Output (not b) (End ()))

{- See how this is the same as the notGate example in Exercise 2, only
   here instead of explicitly giving the two different options for the
   two possible inputs, we give a function that decides what to do
   instead. In this case, it outputs the 'not' of whatever the input
   is. -}

{- Let's have an example process: this process inputs any value, and
   then outputs that same value. -}

echo :: CP x ()
echo = Input (\v -> Output v (End ()))

{- We make processes 'go' in the same way as we did before. We interpret
   them, feeding the 'Input's from a list of inputs, and placing the
   'Output's into a list. There are two main differences with
   'process' from Ex 2: we need to return the extra value attached to
   'End', and we need to interpret the new 'Abort' instruction. -}

process :: CP x a -> [x] -> (Maybe a,[x])
process (End a)      inputs     = (Just a, [])
process (Input k)    (v:inputs) = process (k v) inputs
process (Input k)    []         = (Nothing, [])
process (Output v k) inputs     = (a,v:outputs)
  where (a,outputs) = process k inputs
process Abort        inputs     = (Nothing, [])

{- For example,

   process echo ["Hello"] == (Just (),["Hello"])
-}

{- If we have a process that communicates using 'String's, then we can
   make it actually interact with the user using 'runIO'. This
   function translates process descriptions into I/O commands. This
   function uses Haskell's basic facilites for doing real I/O. We will
   come back to this later in the course. -}

runIO :: CP String a -> IO (Maybe a)
runIO (End a)      = return (Just a)
runIO (Input k)    = getLine >>= runIO . k
runIO (Output x k) = putStrLn x >> runIO k
runIO Abort        = return Nothing

{- Here's an example of using 'runIO'. The '>' is the haskell prompt.

   > runIO echo
   hello
   hello
   Just ()

   where the first 'hello' is typed by the user, and the second is
   printed by the computer. You can use runIO to test your processes
   below, interactively. -}

{- Let's make some basic processes that we can use to build larger
   processes. Your job is to write these from their specifications. -}

{- 3.18 Define 'input'. 'input' is the process that inputs a single
   value and then ends with that value. -}
input :: CP x x
input = undefined

{- 1 mark -}

{- 3.19 Define 'output'. 'output x' is the process that outputs the
   value x, and then ends with the value (). -}
output :: x -> CP x ()
output x = undefined

{- 1 mark -}

{- 3.20 Here is the 'Process' type from Exercise 2, with the
   constructors renamed to not clash with the ones from our new 'CP'
   type. -}

data Process
  = PEnd
  | POutput Bool Process
  | PInput Process Process
  deriving Show

{- Write a function that upgrades a 'Process' into a 'CP Bool
   ()'. Whenever the Process ends, the 'CP' process should end;
   whenever the 'Process' outputs, the 'CP' process should output; and
   whenever the 'Process' inputs, the 'CP' process should input. -}

upgrade :: Process -> CP Bool ()
upgrade = undefined

{- 2 marks -}

{- 3.21 Sequential composition of processes. In the previous exercise,
   sequential composition of processes had type 'Process -> Process ->
   Process'. Here, processes end with a value, which is passed on to
   subsequent processes. Define the rest of this function to complete
   the definition of sequential composition of processes.

   Here are some examples of its use:

   > runIO (Input (\x -> End x) `sequ` \x -> Output x (End ()))
   hello
   hello
   Just ()

   > runIO (Input (\x -> Abort) `sequ` \x -> End ())
   hello
   Nothing
-}

sequ :: CP x a -> (a -> CP x b) -> CP x b
sequ (End a)      f = f a
sequ (Input k)    f = Input undefined
sequ (Output x k) f = undefined
sequ Abort        f = undefined

{- 3 marks -}

{- 3.22 Define a process that does the same thing as 'echo' above, but
   using only 'input', 'output' and 'sequ'. -}

echoFromSequ :: CP x ()
echoFromSequ = undefined

{- 1 mark -}

{- 3.23 Define a process that inputs two numbers and ends with the sum
   of those numbers. -}

addInputs :: CP Int Int
addInputs = undefined

{- 2 marks -}

{- 3.24 Using the 'sequ' function, define cpApply, which takes a process
   that returns a function, a process that returns a value and returns
   a process that returns the result of applying the function to the
   value. It ought to sequence the operations of the two processes so
   that the process that returns the function goes first. -}

cpApply :: CP x (a -> b) -> CP x a -> CP x b
cpApply pf pa = undefined

{- 2 marks -}

{- 3.25 Now write addInputs again, but this time using 'input',
   'cpApply', and End. -}

addInputs2 :: CP Int Int
addInputs2 = undefined

{- 1 mark -}

{----------------------------------------------------------------------}
{- 3.26 WILL BE REVEALED IN THE TEST                                  -}
{- 4 marks -}
{----------------------------------------------------------------------}

{- 3.28 Piping one process's output into another input. Just as for
   'Process'es in Exercise 2, we can plug our processes together in
   parallel, feeding the outputs of one into the inputs of the
   other. Complete the definition of 'pipe', following the same
   specification as in Exercise 2, but noting that (a) if process two
   requires input that process one cannot give, then we abort; and (b)
   we must also handle the Abort cases from both processes. -}

pipe :: CP x a -> CP x b -> CP x b
pipe p1            (End b)       = undefined
pipe p1            (Output x p2) = undefined
pipe (End _)       (Input _)     = Abort
pipe (Output x p1) (Input p2)    = undefined
pipe (Input p1)    p2            = undefined
pipe Abort         _             = undefined
pipe _             Abort         = undefined

{- 5 marks -}

{- 3.29 Knock-knock jokes!

   Exhaustive research has discovered that users love it when their
   computers tell them jokes. It helps people think their machines are
   more than just unfeeling lumps of metal and plastic.

   To bring this vision into the present, you are asked to implement a
   generic "Knock Knock" joke telling process. The protocol for "Knock
   Knock" jokes is very rigid:

   1. Your process must output "Knock, Knock!"
   2. The response must be "Who's there?"
   3. Your process must output the 'who' string it has been provided with
   4. The response must be 'who ++ " who?"'
   5. Your process must output 'who ++ " " ++ clarification'
   6. The response must be "*LOL*"

   If at any point your process detects that the "Knock Knock" joke
   protocol has been violated, it must abort. You might find a nice
   way to package up the idea of 'input with expectations'. -}

knockKnocker :: String -> String -> CP String ()
knockKnocker who clarification =
  undefined

{- 2 marks -}

{----------------------------------------------------------------------}
{- 3.30 - 3.32 WILL BE REVEALED IN THE TEST                           -}
{- 8 marks -}
{----------------------------------------------------------------------}
