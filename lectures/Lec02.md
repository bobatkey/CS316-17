# Lecture 02 : Standard Types and Classes

In the last lecture we introduced the two basic concepts in Haskell
(and functional programming in general): data and transformation of
data by pattern matching. We also introduced the central role of
*types* -- ways of collecting related sorts of data into meaningful
groups. We defined our our type of lists. In this lecture, we will
take a tour of some of the types predefined by Haskell, and introduce
the concept of *type classes*, groupings of related types.

## Introducing GHCi and the REPL

This lecture was entirely conducted by typing Haskell into `ghci`, the
interactive Read-Eval-Print Loop (REPL). The REPL gives us a way to
interactively explore the Haskell system and the programs we write.

From the machines in the Labs, you should be able to type `ghci` at
the terminal to start:

```
abc15999@cafe:~$ ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
Prelude> 
```

The `Prelude>` prompt tells us that `ghci` is waiting for our
input. "Prelude" is the name of the module (part of the standard
library) that contains the basic types and functions used by almost
all Haskell programs and is loaded by default. In general, `ghci`
shows the list of currently loaded modules before the `>`.

We can now start typing things into `ghci` to see what happens. For
instance, lets get it to do some arithmetic. If we type in `2 + 2` and
press 'Enter', `ghci` will print the answer:

```
Prelude> 2 + 2
4
```

Likewise for `2 * 5`:

```
Prelude> 2 * 5
10
```

Now that we have a way to explore Haskell interactively, we can start
to explore the types that come predefined.

## Built-in Types



### Questions to ask of every type

1.  How do we form the type?

2.  What values does the type admit?

3.  How do I use the values? (can I pattern match?)

4.  Is there any fancy syntax for the type, or its values?

5.  Does this type belong to any type classes?



### Booleans

```haskell
Prelude> :info Bool
data Bool = False | True 	-- Defined in ‘GHC.Types’
instance Bounded Bool -- Defined in ‘GHC.Enum’
instance Enum Bool -- Defined in ‘GHC.Enum’
instance Eq Bool -- Defined in ‘GHC.Classes’
instance Ord Bool -- Defined in ‘GHC.Classes’
instance Read Bool -- Defined in ‘GHC.Read’
instance Show Bool -- Defined in ‘GHC.Show’
```

```
Prelude> True
True
Prelude> False
False
```

```
Prelude> True :: Bool
True
```

Using booleans:

```
Prelude> if True then 1 else 2
1
Prelude> if False then 1 else 2
2
```

Note that both sides must have the same type:

```
Prelude> if True then "Hello" else False

<interactive>:2:27:
    Couldn't match expected type ‘[Char]’ with actual type ‘Bool’
    In the expression: False
    In the expression: if True then "Hello" else False
```

Because `Bool` is a normal data type (it is defined using `data`
according to `:info`), we can define functions on it using pattern
matching:

```
Prelude> let not True = False; not False = True
Prelude> not True
False
```

We are now in a position to answer our five questions for the `Bool` type:

1.  We form the type of booleans just by writing `Bool`
2.  There are two values of type `Bool`: `True` and `False`
3.  We can use the values either by `if then else` or by pattern matching
4.  The `if then else` is special syntax that only applies to booleans
5.  According to `:info` the type `Bool` supports `Eq`, `Bounded`,
    `Enum`, `Ord`, `Read` and `Show`. We will see what these mean below.


### Integers

```haskell
Prelude> :info Int
data Int = GHC.Types.I# GHC.Prim.Int# 	-- Defined in ‘GHC.Types’
instance Bounded Int -- Defined in ‘GHC.Enum’
instance Enum Int -- Defined in ‘GHC.Enum’
instance Eq Int -- Defined in ‘GHC.Classes’
instance Integral Int -- Defined in ‘GHC.Real’
instance Num Int -- Defined in ‘GHC.Num’
instance Ord Int -- Defined in ‘GHC.Classes’
instance Read Int -- Defined in ‘GHC.Read’
instance Real Int -- Defined in ‘GHC.Real’
instance Show Int -- Defined in ‘GHC.Show’
```

1.  How do we form the type of machine integers? 

    Simply: `Int`
    
2.  What values inhabit `Int`?

    Integers within a certain range: `1`, `1024`, `-90000`. The range of 
    
3.  How do we use values of type `Int`?



4.  Is there any special syntax?


5.  What type classes does `Int` belong to?

```haskell
Prelude> :info Integer
data Integer
  = integer-gmp-1.0.0.0:GHC.Integer.Type.S# !GHC.Prim.Int#
  | integer-gmp-1.0.0.0:GHC.Integer.Type.Jp# {-# UNPACK #-}integer-gmp-1.0.0.0:GHC.Integer.Type.BigNat
  | integer-gmp-1.0.0.0:GHC.Integer.Type.Jn# {-# UNPACK #-}integer-gmp-1.0.0.0:GHC.Integer.Type.BigNat
  	-- Defined in ‘integer-gmp-1.0.0.0:GHC.Integer.Type’
instance Enum Integer -- Defined in ‘GHC.Enum’
instance Eq Integer
  -- Defined in ‘integer-gmp-1.0.0.0:GHC.Integer.Type’
instance Integral Integer -- Defined in ‘GHC.Real’
instance Num Integer -- Defined in ‘GHC.Num’
instance Ord Integer
  -- Defined in ‘integer-gmp-1.0.0.0:GHC.Integer.Type’
instance Read Integer -- Defined in ‘GHC.Read’
instance Real Integer -- Defined in ‘GHC.Real’
instance Show Integer -- Defined in ‘GHC.Show’
```

Difference between 

### Lists


### `Char`acters


### Strings (are lists of characters)


### Functions

## Type Classes


### Our first type class : `Eq`uality


### `Num`bers and `Ord`ered Types
