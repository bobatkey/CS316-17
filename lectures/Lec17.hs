module Lec17 where

-- import Prelude hiding (Foldable(..))
import Data.Foldable

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

{-
class Foldable f where
     fold :: Monoid m => f m -> m
-}


instance Foldable Tree where
  -- fold :: Monoid m => Tree m -> m
  fold Leaf =
    mempty

  fold (Node l x r) =
    fold l `mappend` x `mappend` fold r   

  foldMap f = fold . fmap f

checkAll :: (Functor c, Foldable c) => (a -> Maybe b) -> c a -> Bool
checkAll checker box = getAll (fold (fmap (checker2 . checker) box))
  where
    -- checker2 :: Maybe b -> All
    checker2 Nothing = MkAll False
    checker2 (Just _) = MkAll True

newtype All = MkAll Bool

getAll :: All -> Bool
getAll (MkAll b) = b

instance Monoid All where
  mempty = MkAll True
  x `mappend` y = MkAll $ (getAll x) && (getAll y)

{- Can we solve our problem now? -}

checkThenConvert :: (Functor c, Foldable c) =>
                    (a -> Maybe b) ->
                    c a ->
                    Maybe (c b)
checkThenConvert checker box =
  if checkAll checker box then
    Just (fmap (fromJust . checker) box)
  else
    Nothing

fromJust :: Maybe b -> b
fromJust (Just b) = b
fromJust Nothing = error "Sorry, you can get refunds from /dev/null"

{- Not ideal! -}

checkThenConvert2 :: (a -> Maybe b) ->
                     Tree a ->
                     Maybe (Tree b)
checkThenConvert2 checker Leaf = pure Leaf
checkThenConvert2 checker (Node l x r) =
  Node <$> checkThenConvert2 checker l
       <*> checker x
       <*> checkThenConvert2 checker r

{- Visiting pattern 3 : Traversable

   A traversable applies a function to every element in the container,
   and (i) creates a new container with the same shape and new
   elements; and (ii) combines results on the side. -}

-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b

{-
class Traversable c where
  traverse :: (Applicative f) =>
              (a -> f b) -> c a -> f (c b)
-}

instance Traversable Tree where
  traverse f Leaf = pure Leaf
  traverse f (Node l x r) =
    Node <$> traverse f l
         <*> f x
         <*> traverse f r
  
{-
instance Traversable [] where
  traverse f [] = pure []
  traverse f (x:xs) = (:) <$> f x <*> traverse f xs
-}



-- foldMap = fold . fmap

{-
   fmap     ::                  (a -> b)   -> c a  -> c b
   foldMap  :: Monoid m      => (a -> m)   -> c a  -> m
   traverse :: Applicative f => (a -> f b) -> c a  -> f (c b)
-}

-- fmap id     ::                  c a     -> c a
-- foldMap id  :: Monoid m =>      c m     -> m
-- traverse id :: Applicative f => c (f a) -> f (c a)

{-
sequenceA :: Applicative f => [f a] -> f [a]
sequenceA = traverse id

sequence :: Monad m => [m a] -> m [a]
sequence = traverse id
-}

-- mapM_ :: (a -> m b) -> [a] -> m ()

-- forM :: [a] -> (a -> m b) -> m ()

-- forM [1..10] (\i -> print i)

-- forM myList (\x -> ...)


