module Lec17 where

{-     LECTURE 17 : VISITING AND TRAVERSING CONTAINERS

   A problem:

     - We have a container full of values 'box :: c a'

     - We have a checking function 'f :: a -> Maybe b'

     - We want to
         (a) find out if all the 'a's in 'box' are OK
         (b) if so, make a new version of 'box' of type 'c b'
-}

{- A container type: -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{- Visiting pattern 1 : Functor

   A functor applies a function to every element in the container, and
   creates a new container with the same shape, but new elements. -}
instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f Leaf         = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

{- Example: -}
convertAll :: Functor c => (a -> Maybe b) -> c a -> c (Maybe b)
convertAll checker box = fmap checker box

{- Visiting pattern 2 : Foldable

   A foldable applies a function to every element in the container,
   and combines all the results. -}
instance Foldable Tree where
  -- foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f Leaf = mempty
  foldMap f (Node l x r) =
    foldMap f l `mappend` f x `mappend` foldMap f r

{-  newtype All = All { getAll :: Bool }

    instance Monoid All where
      mempty = All True
      All p `mappend` All q = All (p && q)
-}

checkAll :: Foldable c => (a -> Maybe b) -> c a -> Bool
checkAll checker box = getAll (foldMap checker2 box) where
  -- checker2 :: a -> All
  checker2 x = case checker x of
                 Nothing -> All False
                 Just b  -> All True
  
{- Can we solve our problem now? -}

checkThenConvert :: (Functor c, Foldable c) =>
                    (a -> Maybe b) ->
                    c a ->
                    Maybe (c b)
checkThenConvert checker box =
  if checkAll checker box then
    Just $ fmap (fromJust . checker) box
  else
    Nothing

fromJust :: Maybe b -> b
fromJust (Just x) = x

{- Not ideal! -}

{- Visiting pattern 3 : Traversable

   A traversable applies a function to every element in the container,
   and (i) creates a new container with the same shape and new
   elements; and (ii) combines results on the side. -}

-- FIXME: introduce this more slowly!
-- write traverse for a Tree first

-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f Leaf         = pure Leaf
  traverse f (Node l x r) = Node <$> traverse f l
                                 <*> f x
                                 <*> traverse f r

convertAndCheckAll :: Traversable c =>
                      (a -> Maybe b)
                   -> c a
                   -> Maybe (c b)
convertAndCheckAll checker box = traverse checker box



{-
   fmap     ::                  (a -> b)   -> c a  -> c b
   foldMap  :: Monoid m      => (a -> m)   -> c a  -> m
   traverse :: Applicative f => (a -> f b) -> c a  -> f (c b)
-}

-- Derived functions

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM = undefined

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA = undefined

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM = undefined
