{-|
 -
Module:      A3
Description: Assignment 3
Copyright: (c) University of Toronto
               CSC324 Principles of Programming Languages, Fall 2023
-}
-- This lists what this module exports. Don't change this!

module A3 (
    -- Warmup Task 1
    racketifyValue, racketifyExpr,
    -- Warmup Task 2
    cpsFactorial, cpsFibonacci, cpsLength, cpsMap,
    cpsMergeSort, cpsSplit, cpsMerge,
    -- Main Task
    cpsEval
) where

-- You *may not* add imports from Data.Map, or any other imports
import qualified Data.Map (Map, lookup, insert, empty, fromList)
import A3Types (Env, emptyEnv, Value(..), Expr(..))

import Data.List (intercalate)

------------------------------------------------------------------------------
-- * Warmup Task. CPS Transforming Haskell Functions *
------------------------------------------------------------------------------

-- | Compute the factorial of a number
-- factorial :: Int -> Int

-- | Compute the factorial of a number, in continuation passing style
cpsFactorial:: Int -> (Int -> r) -> r
cpsFactorial 0 k = k 1
cpsFactorial n k = undefined

-- | Compute the n-th fibonacci number F(n).
--    Recall F(0) = 0, F(1) = 1, and F(n) = F(n-1) + F(n-2)

-- fibonacci :: Int -> Int
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = (fibonacci (n - 1)) + (fibonacci (n - 2))

-- | Compute the n-th fibonacci number F(n), in continuation passing style
cpsFibonacci:: Int -> (Int -> r) -> r
cpsFibonacci n k = undefined

------------------------------------------------------------------------------
-- | List functions

-- | CPS transform of the function `length`, which computes the length of a list
cpsLength :: [a] -> (Int -> r) -> r
cpsLength [] k = undefined


-- | CPS transform of the function `map`. The argument function (to be applied
--   every element of the list) is written in direct style
cpsMap :: (a -> b) -> [a] -> ([b] -> r) -> r
cpsMap f [] k = undefined

------------------------------------------------------------------------------
-- Merge Sort

-- | Sort a list using mergeSort
-- mergeSort :: [Int] -> [Int]

-- | Split a list into two lists. All list elements in even indices
-- are placed in one sub-list, and all list elements in odd indices
-- are placed in the second sub-list.
-- split :: [Int] -> ([Int], [Int])

-- | Merge two sorted lists together
-- merge :: [Int] -> [Int] -> [Int]

-- | CPS transform of mergeSort
cpsMergeSort :: [Int] -> ([Int] -> r) -> r
cpsMergeSort lst k = undefined

-- | CPS transform of split
cpsSplit :: [Int] -> (([Int], [Int]) -> r) -> r
cpsSplit lst k = undefined

-- | CPS transform of merge
cpsMerge :: [Int] -> [Int] -> ([Int] -> r) -> r
cpsMerge lst1 lst2 k = undefined

------------------------------------------------------------------------------
-- * Main Task. CPS Transforming The Orange Interpreter *
------------------------------------------------------------------------------

-- | A CPS interpreter `eval` for Orange , which takes an environment,
--   an expression, and a continuation, and calls the continuation with
--   the evaluated value.
--   Notice that the type signature of `eval` is less general compared to
--   what was used above, i.e., it is not:
--      Env -> Expr -> (Value -> r) -> r
--   This restriction on the type of the continuation makes it easier
--   to define `Expr` Haskell data type, and to check for errors.

cpsEval :: Env -> Expr -> (Value -> Value) -> Value
cpsEval env (Literal v) k = 
    if validLiteral v
        then k v
        else Error "Literal"

cpsEval env (Var name) k = 
    case Data.Map.lookup name env of
        Just v  -> k v
        Nothing -> Error "Var"

cpsEval env (Plus a b) k = 
    cpsEval env a $ \va ->
        case va of 
            Error e -> Error e
            Num x -> cpsEval env b $ \vb ->
                case vb of
                    Error e -> Error e
                    Num y -> k (Num (x + y))
                    _ -> Error "Plus"
            _ -> Error "Plus"

cpsEval env (Times a b) k = 
    cpsEval env a $ \va ->
        case va of 
            Error e -> Error e
            Num x -> cpsEval env b $ \vb ->
                case vb of
                    Error e -> Error e
                    Num y -> k (Num (x * y))
                    _ -> Error "Times"
            _ -> Error "Times"

cpsEval env (Equal a b) k = 
    cpsEval env a $ \va ->
        case va of 
            Error e -> Error e
            _ -> cpsEval env b $ \vb ->
                case vb of
                    Error e -> Error e
                    _ -> k $ if va == vb then T else F

cpsEval env (Lambda params body) k_lambda = k_lambda $ Closure $ \argvals k_app ->
    -- TODO: handle errors!
    -- note that we differentiate between k_lambda: the continuation 
    -- to call after *creating the closure*, and k_app: the continuation
    -- to call after *evaluating the body of the closure* during an
    -- application.
    let paramArgTuples = zip params argvals
        newEnv = foldl (\e (param, arg) -> Data.Map.insert param arg e)
                       env
                       paramArgTuples
    in cpsEval newEnv body k_app
cpsEval env _ k = undefined


-- Helper function (written in direct style) to identify duplicate parameters in a lambda
unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
  | elem x xs = unique xs
  | otherwise = x : unique xs

-- Helper function (written in direct style) to check if a Value contains a Closure/Error
validLiteral :: Value -> Bool
validLiteral T           = True
validLiteral F           = True
validLiteral (Num n)     = True
validLiteral Empty       = True
validLiteral (Pair v w)  = (validLiteral v) && (validLiteral w)
validLiteral (Closure p) = False
validLiteral (Error e)   = False


racketifyValue :: Value -> String
racketifyValue T = "#t"
racketifyValue F = "#f"
racketifyValue (Num x) = show x
racketifyValue Empty = "'()"
racketifyValue (Pair a b) = "(cons " ++ racketifyValue a ++ " " ++ racketifyValue b ++ ")"
racketifyValue (Closure _) = error "can't racketify a closure"
racketifyValue (Error _) = error "can't racketify an error value"

racketifyExpr :: Expr -> String
racketifyExpr (Literal v) = racketifyValue v
racketifyExpr (Plus a b) = undefined
racketifyExpr (Times a b) = undefined
racketifyExpr (Equal a b) = "(equal? " ++ racketifyExpr a ++ " " ++ racketifyExpr b ++ ")"
racketifyExpr (Cons a b) = undefined
racketifyExpr (First a) = undefined
racketifyExpr (Rest a) = undefined
racketifyExpr (Var x) = x
racketifyExpr (If c t f) = undefined
racketifyExpr (Lambda xs body) = "(lambda (" ++ intercalate " " xs ++ ") " ++ racketifyExpr body ++ ")"
racketifyExpr (App f xs) = undefined
racketifyExpr (Shift x e1) = undefined
racketifyExpr (Reset e1) = undefined
