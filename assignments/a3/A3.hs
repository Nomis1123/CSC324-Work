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

cpsFactorial :: Int -> (Int -> r) -> r
cpsFactorial 0 k = k 1
cpsFactorial n k = cpsFactorial (n - 1) $ \res -> k (n * res)

cpsFibonacci :: Int -> (Int -> r) -> r
cpsFibonacci 0 k = k 0
cpsFibonacci 1 k = k 1
cpsFibonacci n k = 
    cpsFibonacci (n - 1) $ \res1 ->
        cpsFibonacci (n - 2) $ \res2 ->
            k (res1 + res2)

cpsLength :: [a] -> (Int -> r) -> r
cpsLength [] k = k 0
cpsLength (x:xs) k = cpsLength xs $ \res -> k (1 + res)

cpsMap :: (a -> b) -> [a] -> ([b] -> r) -> r
cpsMap f [] k = k []
cpsMap f (x:xs) k = 
    cpsMap f xs $ \res -> k (f x : res)

cpsSplit :: [Int] -> (([Int], [Int]) -> r) -> r
cpsSplit [] k = k ([], [])
cpsSplit [x] k = k ([x], [])
cpsSplit (x:y:rest) k = 
    cpsSplit rest $ \(evens, odds) -> 
        k (x:evens, y:odds)

cpsMerge :: [Int] -> [Int] -> ([Int] -> r) -> r
cpsMerge [] ys k = k ys
cpsMerge xs [] k = k xs
cpsMerge (x:xs) (y:ys) k = 
    if x <= y
    then cpsMerge xs (y:ys) $ \res -> k (x:res)
    else cpsMerge (x:xs) ys $ \res -> k (y:res)

cpsMergeSort :: [Int] -> ([Int] -> r) -> r
cpsMergeSort [] k = k []
cpsMergeSort [x] k = k [x]
cpsMergeSort lst k = 
    cpsSplit lst $ \(left, right) ->
        cpsMergeSort left $ \sortedLeft ->
            cpsMergeSort right $ \sortedRight ->
                cpsMerge sortedLeft sortedRight k

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

cpsEval env (Cons a b) k = 
    cpsEval env a $ \va ->
        case va of
            Error e -> Error e
            _ -> cpsEval env b $ \vb ->
                case vb of
                    Error e -> Error e
                    _ -> k (Pair va vb)

cpsEval env (First expr) k =
    cpsEval env expr $ \v ->
        case v of 
            Error e -> Error e
            Pair a b -> k a
            _ -> Error "First"

cpsEval env (Rest expr) k =
    cpsEval env expr $ \v ->
        case v of
            Error e -> Error e
            Pair a b -> k b
            _ -> Error "Rest"

cpsEval env (If cond thenExpr elseExpr) k = 
    cpsEval env cond $ \vc ->
        case vc of
            Error e -> Error e
            F -> cpsEval env elseExpr k
            _ -> cpsEval env thenExpr k

cpsEval env (Reset expr) k = 
    case cpsEval env expr id of
        Error e -> Error e
        v -> k v

cpsEval env (Shift name body) k =
    let capturedCont = \v -> k v
        closureForCont = Closure $ \argvals k_app ->
            case argvals of
                [v] -> k_app (capturedCont v)
                _ -> Error "App"
        newEnv = Data.Map.insert name closureForCont env
    in cpsEval newEnv body id

cpsEval env (Lambda params body) k_lambda = 
    if params /= unique params
    then Error "Lambda"
    else k_lambda $ Closure $ \argvals k_app ->
        if length params /= length argvals
        then Error "App"
        else let paramArgTuples = zip params argvals
                 newEnv = foldl (\e (param, arg) -> Data.Map.insert param arg e)
                                env
                                paramArgTuples
             in cpsEval newEnv body k_app

cpsEval env (App fnExpr argExprs) k = 
    cpsEval env fnExpr $ \vfn ->
        case vfn of
            Error e -> Error e
            Closure f -> 
                evalArgs env argExprs $ \argVals ->
                    f argVals k
            _ -> Error "App"

-- Helper function to evaluate arguments
evalArgs :: Env -> [Expr] -> ([Value] -> Value) -> Value
evalArgs env [] k = k []
evalArgs env (e:es) k = 
    cpsEval env e $ \v ->
        case v of
            Error err -> Error err
            _ -> evalArgs env es $ \vs ->
                k (v:vs)

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
racketifyExpr (Plus a b) = "(+ " ++ racketifyExpr a ++ " " ++ racketifyExpr b ++ ")"
racketifyExpr (Times a b) = "(* " ++ racketifyExpr a ++ " " ++ racketifyExpr b ++ ")"
racketifyExpr (Equal a b) = "(equal? " ++ racketifyExpr a ++ " " ++ racketifyExpr b ++ ")"
racketifyExpr (Cons a b) = "(cons " ++ racketifyExpr a ++ " " ++ racketifyExpr b ++ ")"
racketifyExpr (First a) = "(car " ++ racketifyExpr a ++ ")"
racketifyExpr (Rest a) = "(cdr " ++ racketifyExpr a ++ ")"
racketifyExpr (Var x) = x
racketifyExpr (If c t f) = "(if " ++ racketifyExpr c ++ " " ++ racketifyExpr t ++ " " ++ racketifyExpr f ++ ")"
racketifyExpr (Lambda xs body) = "(lambda (" ++ intercalate " " xs ++ ") " ++ racketifyExpr body ++ ")"
racketifyExpr (App f xs) = "(" ++ racketifyExpr f ++ " " ++ intercalate " " (map racketifyExpr xs) ++ ")"
racketifyExpr (Shift x e1) = "(shift " ++ x ++ " " ++ racketifyExpr e1 ++ ")"
racketifyExpr (Reset e1) = "(reset " ++ racketifyExpr e1 ++ ")"
