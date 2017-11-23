module Tut09 where

{-    TUTORIAL 09 : MORE EVALUATORS -}



newtype Reader s a = MkReader (s -> a)
  --                          (s -> (s,a))

instance Functor (Reader s) where
  -- fmap :: (a -> b) -> Reader s a -> Reader s b
  fmap f (MkReader g) = MkReader (\s -> f (g s))

instance Applicative (Reader s) where
  pure x = MkReader (\s -> x)

  MkReader rf <*> MkReader ra =
    MkReader (\s -> rf s (ra s))

instance Monad (Reader s) where
  MkReader ra >>= f =
    MkReader (\s ->
                let a = ra s
                    MkReader g = f a
                in g s)

runReader :: Reader s a -> s -> a
runReader (MkReader g) s = g s

ask :: Reader s s
ask = MkReader (\s -> s)

data ReaderExpr
  = Number Int
  | Add    ReaderExpr ReaderExpr
  | Var    String
  deriving Show

data Val
  = ValInt Int
  | ValRef String

type Env = [(String,Val)]

lookupEnv :: String -> Env -> Maybe Int
lookupEnv x env =
  case lookup x env of
    Nothing         -> Nothing
    Just (ValInt i) -> Just i
    Just (ValRef x) -> lookupEnv x env

-- M.Map String Int
evalReaderExpr :: ReaderExpr -> Reader Env Int
evalReaderExpr (Number n)  = pure n
evalReaderExpr (Add e1 e2) = (+) <$> evalReaderExpr e1 <*> evalReaderExpr e2
evalReaderExpr (Var x)     = do env <- ask
                                case lookupEnv x env of
                                  Nothing -> pure 0
                                  Just v  -> pure v


{----------------------------------------------------------------------}
{-    PART II : STATE -}

newtype State s a = MkState (s -> (a,s))

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


get :: State a a
get = MkState (\ a -> (a,a))

put :: s -> State s ()
put s = MkState (\ _ -> ((), s))

runState :: State s a -> s -> (a,s)
runState (MkState h) s = (h s)

evalState :: State s a -> s -> a
evalState h = fst . runState h

data StateExpr
  = StNumber Int
  | StAdd StateExpr StateExpr
  | StGet
  | StPut StateExpr StateExpr
  deriving Show

evalStateExpr :: StateExpr -> State Int Int
evalStateExpr (StNumber n)  = pure n
evalStateExpr (StAdd e1 e2) = (+) <$> evalStateExpr e1 <*> evalStateExpr e2
evalStateExpr StGet         = get
evalStateExpr (StPut e1 e2) = do
  n <- evalStateExpr e1
  put n
  evalStateExpr e2
