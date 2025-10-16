{-|
Module: A2
Description: Assignment 2
Copyright: (c) University of Toronto Mississagua
               CSC324 Principles of Programming Languages, Fall 2025
-}
-- This lists what this module exports. Don't change this!
module A2
  (
    run,
    eval
  )
where

-- You *may not* add imports from Data.Map, or any other imports
import A2Types(Expr(..), Value(..), Env)
import qualified Data.Map (lookup, insert, empty)
import Data.List (intercalate)

-- | Runs an Orange expression by calling `eval` with the empty environment
run :: Expr -> Value
run e = eval Data.Map.empty e


-- | An interpreter for the Orange language.
eval :: Env -> Expr -> Value
eval env (Literal (Closure _ _ _)) = Error "Literal"
eval env (Literal (Error _)) = Error "Literal"
eval env (Literal v) = v

eval env (Plus a b)  = case ((eval env a), (eval env b)) of
    (Num x, Num y) -> Num (x + y)
    (Error e, _) -> Error e
    (_, Error e) -> Error e
    _ -> Error "Plus"

eval env (Times a b) = case ((eval env a), (eval env b)) of
    (Num x, Num y) -> Num (x * y)
    (Error e, _) -> Error e
    (_, Error e) -> Error e
    _ -> Error "Times"

eval env (Equal a b) = case ((eval env a), (eval env b)) of
    (Error e, _) -> Error e
    (_, Error e) -> Error e
    (x, y) -> if x == y then T else F
      
eval env (Var name)  = case (Data.Map.lookup name env) of
    Just a  -> a
    Nothing -> Error "Var"

eval env (If c t x) = case (eval env c) of
    Error e -> Error e
    F       -> eval env x
    T       -> eval env t
    _       -> Error "If"

eval env (Lambda args body) = 
    if length args /= length (unique args) 
    then Error "Lambda"
    else Closure args env body

eval env _ = Error "Not implemented yet"


-- | Helper function to obtain a list of unique elements in a list
-- Example:
--   ghci> unique [1, 2, 3, 4]
--   [1,2,3,4]
--   ghci> unique [1, 2, 3, 4, 4]
--   [1,2,3,4]
unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
  | elem x xs = unique xs
  | otherwise = x : unique xs


racketifyValue :: Value -> String
racketifyValue T = "#t"
racketifyValue F = "#f"
racketifyValue (Num x) = show x
racketifyValue Empty = "'()"
racketifyValue (Pair a b) = "(cons " ++ racketifyValue a ++ " " ++ racketifyValue b ++ ")"
racketifyValue (Closure _ _ _) = error "can't racketify a closure"
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