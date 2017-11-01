module Lec17 where

{-    LECTURE 17 : TRAVERSING CONTAINERS -}

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM = undefined

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA = undefined

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM = undefined
