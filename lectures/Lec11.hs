module Lec11 where

{-    LECTURE 11 : BUILDING PURE EVALUATORS -}

{-    PART I : EVALUATION -}

data Expr
  = Number Int
  | Add    Expr Expr
  deriving Show

myExpr :: Expr
myExpr = Number 23 `Add` (Number 34 `Add` Number 56)

evaluate :: Expr -> Int
evaluate (Number n)  = n
evaluate (Add e1 e2) = (+) (evaluate e1) (evaluate e2)

{-    PART II : EVALUATION WITH EXCEPTIONS -}

data Expr2
  = Number2 Int
  | Add2    Expr2 Expr2
  | Throw2
  | Catch2  Expr2 Expr2
  deriving Show

myProblemProgram :: Expr2
myProblemProgram =
  (Number2 23 `Add2` (Number2 34 `Add2` Throw2)) `Catch2` (Number2 0)

evaluate2 :: Expr2 -> Maybe Int
evaluate2 (Number2 n)    = pure n
evaluate2 (Add2 e1 e2)   = pure (+) <*> evaluate2 e1 <*> evaluate2 e2
  {-case evaluate2 e1 of
      Nothing -> Nothing
      Just n1 -> case evaluate2 e2 of
                   Nothing -> Nothing
                   Just n2 -> Just (n1+n2) -}
evaluate2 Throw2         = Nothing
evaluate2 (Catch2 e1 e2) = case evaluate2 e1 of
                             Nothing -> evaluate2 e2
                             Just n  -> Just n

-- maybeApply (Just (+)) :: Maybe Int -> Maybe (Int -> Int)

maybeApply :: Maybe (a -> b) -> Maybe a -> Maybe b
maybeApply (Just f) (Just a) = Just (f a)
maybeApply Nothing  _        = Nothing
maybeApply _        Nothing  = Nothing

maybePure :: a -> Maybe a
maybePure a = Just a

{-     PART III : EVALUATION WITH PRINTING -}

data Expr3
  = Number3 Int
  | Add3    Expr3 Expr3
  | Print3  String Expr3
  deriving Show

printingProg :: Expr3
printingProg =
  (Print3 "Hello" (Number3 23)) `Add3`
  (Number3 34 `Add3` (Print3 " World" (Number3 56)))

evaluate3 :: Expr3 -> (String, Int)
evaluate3 (Number3 n)  = printPure n
evaluate3 (Add3 e1 e2) =
  printPure (+) `printApply` evaluate3 e1 `printApply` evaluate3 e2

  -- (s1 ++ s2, n1 + n2)
  -- where (s1, n1) = evaluate3 e1
  --       (s2, n2) = evaluate3 e2
evaluate3 (Print3 s e) = (s ++ s1, n)
  where (s1, n) = evaluate3 e

printApply :: (String, a -> b) -> (String, a) -> (String, b)
printApply (s1, f) (s2, a) = (s1 ++ s2, f a)

printPure :: a -> (String,a)
printPure a = ("",a)

{-
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
-}

-- fmap  ::   (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b

evaluateApp :: Applicative f => Expr -> f Int
evaluateApp (Number n)  = pure n
evaluateApp (Add e1 e2) = pure (+) <*> evaluateApp e1 <*> evaluateApp e2

{-     PART IV : EVALUATION WITH CHOICE -}

data Expr4
  = Number4 Int
  | Add4    Expr4 Expr4
  | Choice  Expr4 Expr4
  deriving Show

-- if (*) {
--     STATEMENT 1;
-- } else {
--     STATEMENT 2;
-- }

myDitheringProgram :: Expr4
myDitheringProgram =
  ((Number4 23 `Choice` Number4 32)
   `Add4`
   ((Number4 34 `Add4` Number4 56)
    `Choice`
    (Number4 23 `Add4` Number4 34)))

-- (23 || 32) + ((34 + 56) || (23 +34))

-- (1 || 2) + (3 || 4)

evaluate4 :: Expr4 -> [Int]
evaluate4 (Number4 n)  = pure n
evaluate4 (Add4 e1 e2) = pure (+) <*> evaluate4 e1 <*> evaluate4 e2
-- [ n1 + n2 | n1 <- ns1, n2 <- ns2 ]
--   where ns1 = evaluate4 e1
--         ns2 = evaluate4 e2
evaluate4 (Choice e1 e2) = ns1 ++ ns2
  where ns1 = evaluate4 e1
        ns2 = evaluate4 e2

