# Tutorial 8 Exercises

For these exercises, you will need the `State s` monad we defined in
[Lecture 16](Lec16.hs). The idea is that a computation of type `State
s a` returns values of type `a` while also being able to read and
write state of type `s`. This is modelled using the Haskell type:


```haskell
newtype State s a = MkState (s -> (a,s))
```

The `Functor`, `Applicative`, and `Monad` instances are:

```haskell
instance Functor (State s) where
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap f (MkState h) =
    MkState (\s -> let (a,s') = h s in (f a, s'))

instance Applicative (State s) where
  -- pure :: a -> State s a
  pure x = MkState (\s -> (x,s))

  -- <*> :: State s (a -> b) -> State s a -> State s b
  (MkState hg) <*> (MkState ha) =
    MkState (\s -> let (g, s1) = hg s in
                   let (a, s2) = ha s1 in
                   (g a, s2))

instance Monad (State s) where
  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  (MkState ha) >>= f =
    MkState (\s -> let (a, s1) = ha s in
                   let MkState h = f a in
                     h s1)
```

The two operations for manipulating the state are:
```haskell
get :: State a a
get = MkState (\ a -> (a,a))

put :: s -> State s ()
put s = MkState (\ _ -> ((), s))
```

We can run stateful computations with `runState` and `evalState`:
```haskell
runState :: State s a -> s -> (a,s)
runState (MkState h) s = (h s)

evalState :: State s a -> s -> a
evalState h = fst . runState h
```

## State helper functions.

**(a)** Write a function `modify :: (s -> s) -> State s ()` which
updates the state to the result of applying the given function to the
current state. Write it using `get`, `put` and `do`-notation. What is
a good test case to make sure it works?

Conversely, can you think of a way to implement `put` in terms
of `modify`?

**(b)** Write a function `gets :: (s -> a) -> State s a` which gets
the current state and returns the result of applying the given
function to it. Write it using `get`, `return`, and `do`-notation.
What is a good test case to make sure it works?

Conversely, can you think of a way to write `get` in terms of
`gets`?

**(c)** We can maintain a state of two values by storing a tuple in
the state. Write functions `getFst :: State (s, t) s` and
`getSnd :: State (s, t) t` for getting each component, and
functions `putFst :: s -> State (s, t) ()` and
`putSnd :: t -> State (s, t) ()` for updating the value in each
component. What is a good test case to make sure it works?

## Tree Relabeling

Recall the function `labelTree :: Tree a -> Tree Int` from Lecture 16,
that converted a given tree into a tree with integers at the nodes,
storing a "running total". For instance, if

```haskell
testTree = (Node (Node (Node Leaf 'a' Leaf) 'b' (Node Leaf 'c' Leaf))
                 'd'
                 (Node (Node Leaf 'a' Leaf) 'd' (Node Leaf 'b' Leaf)))
```

then we want `labelTree testTree` to evaluate to

```haskell
          (Node (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf))
                   3
                   (Node (Node Leaf 4 Leaf) 5 (Node Leaf 6 Leaf)))
```

**(a)** Rewrite `labelTree` from first principles, without using the
state monad.

Hint: You will probably find it useful to actually write a
function `labelTreeWithLabels :: Int -> Tree a -> (Int, Tree Int)`
which takes as input the label to use, and returns as
output the next available label together with the tree.

**(b)** Write `labelTree` using the state monad as in the lecture, i.e.
write it by first writing a function
`labelTreeState :: Tree a -> State Int (Tree Int)` and then using
`evalState`.

**(c)** Write `labelTree` using the `Traversable` instance for trees,
and the state monad. You will need to use `evalState` again
to extract a tree.

**(d)** We modify the exercise as follows: instead of blindly
increasing the counter at each node, we now demand that the same
element is replaced by the same number at each occurrence, and
when we meet an as-yet-unvisited element we have to find a 'new'
number to match it with. Modify your favourite answer from
(a)--(c) to match the new requirements, and write a new function
`labelTree2`.  With `testTree` as above, we should have
`labelTree2 testTree` evaluate to

```haskell
          (Node (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf))
                   3
                   (Node (Node Leaf 0 Leaf) 3 (Node Leaf 1 Leaf)))
```

Hint: What state do we need to keep track of now?

## Evaluating Expressions with State

Here is a data type describing the syntax of a small programming
language with numbers, addition, and the ability to get and put the
current state.

```haskell
data Expr
  = Number Int
  | Add Expr Expr
  | Get
  | Put Expr Expr
  deriving Show
```

The idea is that `Number` and `Add` are the same as before: numbers
and adding of the results of expressions. The two new constructors
are: `Get`, which should return the current state, and `Put`, which
runs its first argument, updates the current state to be the result of
the first one, and then runs the second one.

**(a)** Write an evaluator with the type:

```haskell
evalExpr1 :: Expr -> Int -> (Int, Int)
```

where the first argument is the expression to be evaluated and the
second argument is the initial state. In the pair that is returned,
the first element ought to be the computed result, and the second is
the final state.

**(b)** Rewrite the evaluator you wrote for Question 1 using the `State s`
monad. You should end up with a function that has the type:

```haskell
evalExpr2 :: Expr -> State Int Int
```

Your two implementations should agree in the sense that for all
`Expr`s `e`, and initial states `i`, `evalExpr1 e i = runState
(evalExpr2 e) i`.
