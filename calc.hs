{- --------------------------------------------------------------------
 - Datatype: MathExpr
 - --------------------------------------------------------------------
 - Description: An Abstract Syntax Tree (AST) for encoding mathematical
 -              expressions
 - Example: The expression
 -                (abs (2*X + 1)) ^ 3
 -          can be encoded as
 -                Power 3 (Func1 Abs
 -                              (Func2 Add (Func2 Mult (Coef 2) X)
 -                                         (Coef 1)))
 - --------------------------------------------------------------------
 -}
 data MathExpr a =
    X
  | Coef a
  | Add (MathExpr a) (MathExpr a)
  | Mult (MathExpr a) (MathExpr a)
  | Power (MathExpr a) Int
  | Cos (MathExpr a)
  | Sin (MathExpr a)
  | Abs (MathExpr a)
  deriving (Eq,Show,Read)

{- -----------------------------------------------------------------
 - eval
 - -----------------------------------------------------------------
Description:
 -  A function that takes input of e which is an expression of type MathExpr, and v which
    is a floating point number. It then evaluates the expression by replacing the 
    variables in the expression with v, to return an output of type a. It 
    essentially evaluates any expression with the custom type MathExpr in
    mind.
 -}
 
eval :: (Floating a, Eq a) => MathExpr a -> a -> a
eval e v = case e of
            X -> v
            Coef a -> a
            Add a b -> (eval a v) + (eval b v)
            Mult a b -> (eval a v) * (eval b v)
            Power a n -> (eval a v) ^^ n
            Cos a -> cos (eval a v)
            Sin a -> sin (eval a v)
            Abs a -> abs (eval a v)

{- -----------------------------------------------------------------
 - instance Num a => Num (MathExpr a)
 - -----------------------------------------------------------------
 Description:
 -  An object of the Num class which takes essentially implements certain mathematical
    operations with the format of the type MathExpr in mind. It takes an input of 
    type a which is an instanceof the num class and returns a value of type a after 
    applying the respective operation.
 -}
 
instance Num a => Num (MathExpr a) where
  x + y         = Add x y
  x * y         = Mult x y
  negate x      = Mult x (Coef (-1))
  abs x         = Abs x
  fromInteger i = Coef (fromInteger i)
  signum _      = error "signum is left un-implemented"

{- -----------------------------------------------------------------
 - instance Fractional a => Fractional (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -  An object of the Fractional class which takes essentially implements the mathematical
    operations of reciprical and fromRational with the format of the type MathExpr in mind. 
    It takes an input of type a which is an instance of the fractional class and returns a value 
    of type a after applying the required operation.
 -}
 
instance Fractional a => Fractional (MathExpr a) where
  recip e        = Power e (-1)
  fromRational e = Coef (fromRational e)

{- -----------------------------------------------------------------
 - instance Floating a => Floating (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -  An object of the Floating class which takes essentially implements trigonometric mathematical
    operations whilst applying the format of the type MathExpr. It takes an input of type a which 
    is an instance of the floating class and returns a value of type a after applying the required 
    operation.
 -}
 
instance Floating a => Floating (MathExpr a) where
  pi      = Coef (pi)
  sin     = Sin
  cos     = Cos
  log     = error "log is left un-implemented"
  asin _  = error "asin is left un-implemented"
  acos _  = error "acos is left un-implemented"
  atan _  = error "atan is left un-implemented"
  sinh _  = error "sinh is left un-implemented"
  cosh _  = error "cosh is left un-implemented"
  tanh _  = error "tanh is left un-implemented"
  asinh _ = error "asinh is left un-implemented"
  acosh _ = error "acosh is left un-implemented"
  atanh _ = error "atanh is left un-implemented"
  exp _   = error "exp is left un-implemented"
  sqrt _  = error "sqrt is left un-implemented"

{- -----------------------------------------------------------------
 - diff
 - -----------------------------------------------------------------
 - Description:
 -  A function that takes an input of e which is an expression of type MathExpr and 
    then symbolically differentiates e by applying the respective differential rule
    to return an output of type MathExpr. It essentially differentiates any expression 
    with the custom type MathExpr in mind.
 -}
 
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff e = case e of
          X -> 1
          Coef _ -> 0
          Add u v -> diff u + diff v
          Mult u v -> diff u * v + u * diff v
          Power u n -> fromIntegral n * (u ^^ (n-1)) * diff u
          Cos u -> sin u * Coef (-1) * diff u
          Sin u -> cos u  * diff u
          Abs u -> (recip (Abs u)) * u * diff u

{- -----------------------------------------------------------------
 - pretty
 - -----------------------------------------------------------------
 - Description:
 -  A function which takes an input e of type MathExpr and then converts e into a written 
    format which is easier to understand, outputting this as a string.
 -}

pretty :: (Show a) => MathExpr a -> String
pretty e = case e of
            X -> "X"
            Coef a -> "(" ++ show a ++ ")"
            Add a b -> "(" ++ pretty a ++ " + " ++ pretty b ++ ")"
            Mult a b -> "(" ++ pretty a ++ " * " ++ pretty b ++ ")"
            Power a n -> "(" ++ pretty a ++ " ^^ (" ++ show n ++ "))"
            Cos a -> "cos(" ++ pretty a ++ ")"
            Sin a -> "sin(" ++ pretty a ++ ")"
            Abs a -> "abs(" ++ pretty a ++ ")"


{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------

Function: eval
Test Case Number: 1
Input: eval (Sin X) (-1)
Expected Output: -0.8414709848
Actual Output: -0.8414709848078965

Function: eval
Test Case Number: 2
Input: eval (Power X 3) (5)
Expected Output: 125.0
Actual Output: 125.0

Function: eval
Test Case Number: 3
Input: eval (Mult X X) (eval (Abs X) (-6))
Expected Output: 36.0
Actual Output: 36.0

Function: diff
Test Case Number: 1
Input: diff (X)
Expected Output: Coef 1.0
Actual Output: Coef 1.0

Function: diff
Test Case Number: 2
Input: diff (Mult X 5)
Expected Output: Add (Mult (Coef 1.0) (Coef 5.0)) (Mult X (Coef 0.0))
Actual Output: Add (Mult (Coef 1.0) (Coef 5.0)) (Mult X (Coef 0.0))

Function: diff
Test Case Number: 3
Input: diff (Cos 5)
Expected Output: Mult (Mult (Sin (Coef 5.0)) (Coef (-1.0))) (Coef 0.0)
Actual Output: Mult (Mult (Sin (Coef 5.0)) (Coef (-1.0))) (Coef 0.0)

Function: pretty
Test Case Number: 1
Input: pretty (Add X X)
Expected Output: "(X + X)"
Actual Output: "(X + X)"

Function: pretty
Test Case Number: 2
Input: pretty (Power X 5)
Expected Output: "(X ^^ (5))"
Actual Output: "(X ^^ (5))"

Function: pretty
Test Case Number: 3
Input: pretty (Mult (Coef 5.0) (Cos(X)))
Expected Output: "((5.0) * cos(X))"
Actual Output: "((5.0) * cos(X))"
 
 -}

infix 4 =~
(=~) :: (Floating a,Ord a) => a -> a -> Bool
x =~ y = abs (x - y) <= 1e-4

{- EXAMPLE
 - Function: eval
 - Property: eval (Func2 Add (Coef x) X) y is correct for all x,y
 - Actual Test Result: Pass
 -}
 
evalProp0 :: (Float,Float) -> Bool
evalProp0 (x,y) = (x + y) =~ eval (Add (Coef x) X) y

runEvalProp0 :: IO ()
runEvalProp0 = quickCheck  evalProp0

-- TODO add more quickcheck test cases here

{- evalProp1
 - Function: eval
 - Property: eval (Func2 Mult (Coef x) X) y is correct for all x,y
 - Actual Test Result: Pass
 -}

evalProp1 :: (Float,Float) -> Bool
evalProp1 (x,y) = (x * y) =~ eval (Mult (Coef x) X) y

runEvalProp1 :: IO ()
runEvalProp1 = quickCheck  evalProp1

{- diffProp1
 - Function: diff
 - Property: diff (Add X (Coef x)) is correct for all x
 - Actual Test Result: Pass
 -}

diffProp1 :: Float -> Bool
diffProp1 x = Coef 1.0 + Coef 0.0 == diff (Add X (Coef x))

runDiffProp1 :: IO ()
runDiffProp1 = quickCheck  diffProp1