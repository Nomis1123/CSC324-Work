{-|
Module:      A3Orange
Description: Reference Orange interpreter for Assignment 3
Copyright: (c) University of Toronto, 2023
               CSC324 Principles of Programming Languages, Fall 2023

This module contains the (non-CPS) version of the Orange interpreter, along
Do not write any of your solutions in this file.
-}

import qualified Data.Map (Map, lookup, insert, empty, fromList)

------------------------------------------------------------------------------
-- Data Definitions: the (non-CPS) Orange language
------------------------------------------------------------------------------

-- | An environment is a mapping of names to values
type Env = Data.Map.Map String Value
emptyEnv = Data.Map.empty

-- | Values in Orange include:
data Value = T | F                -- booleans true and false
           | Num Int              -- integers
           | Empty                 -- the empty list
           | Pair Value Value     -- pairs
           | Closure ([Value] -> Value) -- closures, see below
           | Error String         -- errors
           deriving (Show, Eq)

-- | In order for Value to be members of the Show and Eq typeclasses,
--   we need ([Value] -> Value) to be members of those typeclasses.
--   Thus, we provide an implementation of "show" and "eq" for this type
instance Show ([Value] -> Value) where  
    show x = "[Can't Show Procedure]"
instance Eq ([Value] -> Value) where  
    x == y = False

-- | Expressions in Orange include:
data Expr = Literal Value           -- literal values
          | Plus Expr Expr          -- builtin "plus" function
          | Times Expr Expr         -- builtin "times" function
          | Equal Expr Expr         -- builtin checks for equality
          | Cons Expr Expr          -- builtin "cons" function that creates a pair
          | First Expr              -- builtin "first" function that obtains the first element of a pair
          | Rest Expr               -- builtin "rest" function that obtains the second element of a pair
          | Var String              -- variable names
          | If Expr Expr Expr       -- if expressions
          | Lambda [String] Expr    -- function definitions
          | App Expr [Expr]         -- function applications
          deriving (Eq, Show)

------------------------------------------------------------------------------
-- Interpreter
------------------------------------------------------------------------------

-- | The interpreter `eval` for Orange, which takes an environment
--   and an expression, and returns the evaluated value.
eval :: Env -> Expr -> Value
eval env (Literal v) = if validLiteral v then v else Error "Literal"
eval env (Plus a b)  = case ((eval env a), (eval env b)) of
    (Num x, Num y) -> Num (x + y)
    (Error a, _)   -> Error a
    (_, Error b)   -> Error b
    _              -> Error "Plus"
eval env (Times a b) = case ((eval env a), (eval env b)) of
    (Num x, Num y) -> Num (x * y)
    (Error c, _)   -> Error c
    (_, Error d)   -> Error d
    (c, d)         -> Error "Times"
eval env (Equal a b) = case ((eval env a), (eval env b)) of
    (Error c, _)   -> Error c
    (_, Error d)   -> Error d
    (c, d)         -> if c == d then T else F
eval env (Cons a b) =  case ((eval env a), (eval env b)) of
    (Error c, _)   -> Error c
    (_, Error d)   -> Error d
    (c, d)         -> Pair c d
eval env (First expr) = case (eval env expr) of
    (Pair a b) -> a
    (Error c)  -> Error c
    _          -> Error "First"
eval env (Rest expr) = case (eval env expr) of
    (Pair a b) -> b
    (Error c)  -> Error c
    _          -> Error "Rest"
eval env (If cond expr alt) = case (eval env cond) of
    (Error c)  -> Error c
    F          -> (eval env alt)
    _          -> (eval env expr)
eval env (Var name)  = case (Data.Map.lookup name env) of
    Just a  -> a
    Nothing -> Error "Var"
eval env (Lambda params body) =  
    if params == unique params
    then Closure $ \vargs ->
        if length params == length vargs
        then let paramArgTuples = zip params vargs
                 newEnv = foldl (\e (param, arg) -> Data.Map.insert param arg e)
                                env
                                paramArgTuples
            in eval newEnv body
        else Error "App"
    else Error "Lambda"
eval env (App proc args) = case (eval env proc) of
    Closure f -> let vargs = (map (eval env) args)
                     firstError = foldl combineError Nothing vargs
                 in case firstError of 
                     Just err -> err
                     Nothing  -> f vargs
    Error e   -> Error e
    _         -> Error "App"


-- Helper function that combines two (Maybe Value)s.
-- Returns `Nothing` if neither values are (Error _)
-- Returns the error if one of those values is (Error _)
combineError :: Maybe Value -> Value -> Maybe Value
combineError Nothing (Error v) = Just (Error v)
combineError acc     val       = acc

-- Helper function to identify duplicate parameters in a lambda
unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
  | elem x xs = unique xs
  | otherwise = x : unique xs

-- Helper function to check if a Value contains a Closure/Error
validLiteral :: Value -> Bool
validLiteral T           = True
validLiteral F           = True
validLiteral Empty       = True
validLiteral (Num n)     = True
validLiteral (Pair v w)  = (validLiteral v) && (validLiteral w)
validLiteral (Closure p) = False
validLiteral (Error e)   = False

------------------------------------------------------------------------------
-- Environment Definition
------------------------------------------------------------------------------

-- | Create a new environment from a set of bindings (pairs of names to
--   expressions to be evaluated). There is a tricky bit of Haskell here
--   that supports *recursion* in these definitions!
--   In other words, the expression might reference names that are 
--   being defined. In order to support recursion, notice that we
--   are passing the `env` (currently being created) as the environment
--   in the call to `eval`! This is similar to how we use `letrec` in
--   Racket, but relies on Haskell's lazy evaluation.
def :: [(String, Expr)] -> Env
def bindings = 
    let env = Data.Map.fromList (map (\(n,e) -> (n, (eval env e))) bindings)
    in env

------------------------------------------------------------------------------
-- Example Orange Programs
------------------------------------------------------------------------------

-- | Example: apply the identity function to the number 3
example1 = eval emptyEnv (App (Lambda ["a"] (Var "a")) [Literal $ Num 3])

-- | Example: apply a function that returns 10 plus the second argument
--            to the arguments [1, 2]
example2 = eval emptyEnv (App (Lambda ["a", "b"] (Plus (Literal $ Num 10) (Var "b")))
                              [Literal $ Num 1, Literal $ Num 2])
-- | Example: if statement expression
example3 = eval emptyEnv (If (Equal (Literal F) (Literal F))
                             (Literal T)
                             (Literal F))
-- | Example: creating a function using `def`
sub1Env = def [("sub1", Lambda ["n"] (Plus (Var "n") (Literal $ Num (-1))))]
example4 = eval sub1Env (App (Var "sub1") [Literal $ Num 5])

---- | Example: a recursive factorial definition
facEnv = def [("fac", Lambda ["n"]
                (If (Equal (Var "n") (Literal $ Num 0))
                    (Literal $ Num 1)
                    (Times (Var "n") (App (Var "fac")
                       [(Plus (Var "n") (Literal $ Num (-1)))]))))]
example5 = eval facEnv (App (Var "fac") [Literal $ Num 5])

------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

main :: IO ()
main = do
    print example1
    print example2
    print example3
    print example4
    print example5
