  1. [H7.1] Show how the list comprehension `[ f x | x <- xs, p x]`
      can be re-expressed using the higher-order functions `map` and
      `filter`.

  2. [H8.3--4] Consider the following type of binary trees:

      ```haskell
      data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq)
      ```

      Let us say that such a tree is *balanced* if the number of leaves
      in the left and right subtree of every node differs by at most
      one, with leaves themselves being trivially balanced.

      (a) Define a function `balanced :: Tree a -> Bool` that decides
      if a binary tree is balanced or not.

      Hint: first define a function that returns the number of leaves
      in a tree.

      (b) Define a function `fromList :: [a] -> Tree a` that converts a
      non-empty list into a balanced tree.

      Hint: first define a function that splits a list into two halves
      whose length differs by at most one.

      (c) Define a function `balance :: Tree a -> Tree a` that balances
      a tree, i.e. returns a balanced tree with the same elements as
      the input tree.

  3. The edit distance between two strings is the minimal number of
      operations needed to turn one into the other. By operations, we
      mean "insert a character", "delete a character", "replace a
      character", "keep a character" (this is a zero-cost operation),
      and "kill the rest of the line".

      (a) Write a program that computes the smallest sequence of edits
      that transforms a given string into another given string.

      Hint: This exercise is deliberately phrased at a high level for
      you to practice "functional problem solving skills". If you get
      stuck, or don't know where to start, the following hints might
      help you, but you might enjoy yourself more if you try without
      them at first!

      Hint: How would you represent edits? How would you represent
      sequences of edits? What is the type of the function we want to
      write? Can you think of some way of dividing the problem into
      smaller problems?

      Hint: What are some good test cases that you can try your
      function on?

      Hint: It can be helpful for debugging to have a function which
      takes a sequence of edits and an initial word, and produces a
      sequence of words that arise from applying the edits.

      Hint: Base cases first; how do you transform the empty string
      into the empty string? How do you transform a non-empty string
      into an empty string? How do you transform an empty string into
      a non-empty string?

      Hint: How do you transform a string into another string, if they
      agree on the first character?

      Hint: Otherwise, there is no obvious way to decide if you want
      to insert into, delete from, or change a character in the first
      string to produce the second one. You'll have to try all
      options, see which one has the lowest cost, and go with that
      one.

      Hint: How do you compute the cost of a sequence of edits?

      Hint: Given a cost function, how do you find the best sequence
      of edits among several such?

      (b) Modify the program to accomodate a Swap operation, which can
      be used to transform "abxyz" into "baxyz" in a single step.

      Hint: How would you modify your edit representation?

      Hint: What needs to be changed in the "edit simulator"? In the
      program computing the transformation needed?
