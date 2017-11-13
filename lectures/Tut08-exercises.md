   1. State helper functions.

      (a) Write a function `modify :: (s -> s) -> State s ()` which
      updates the state to the result of applying the given function
      to the current state. Write it using `get`, `put` and
      `do`-notation. What is a good test case to make sure it works?

      Conversely, can you think of a way to implement `put` in terms
      of `modify`?

      (b) Write a function `gets :: (s -> a) -> State s a` which gets
      the current state and returns the result of applying the given
      function to it. Write it using `get`, `return`, and `do`-notation.
      What is a good test case to make sure it works?

      Conversely, can you think of a way to write `get` in terms of
      `gets`?

      (c) We can maintain a state of two values by storing a tuple in
      the state. Write functions `getFst :: State (s, t) s` and
      `getSnd :: State (s, t) t` for getting each component, and
      functions `putFst :: s -> State (s, t) ()` and
      `putSnd :: t -> State (s, t) ()` for updating the value in each
      component. What is a good test case to make sure it works?

  2. Recall the function `labelTree :: Tree a -> Tree Int` from
     Lecture 16, that converted a given tree into a tree with integers
     at the nodes, storing a "running total". For instance, if

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

     (a) Rewrite `labelTree` from first principles, without using the
     state monad.

     Hint: You will probably find it useful to actually write a
     function `labelTreeWithLabels :: Int -> Tree a -> (Int, Tree Int)`
     which takes as input the label to use, and returns as
     output the next available label together with the tree.

     (b) Write `labelTree` using the state monad as in the lecture, i.e.
     write it by first writing a function
     `labelTreeState :: Tree a -> State Int (Tree Int)` and then using
     `evalState`.

     (c) Write `labelTree` using the `Traversable` instance for trees,
     and the state monad. You will need to use `evalState` again
     to extract a tree.

     (d) We modify the exercise as follows: instead of blindly
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
