{-# LANGUAGE FlexibleInstances #-}

{-|
Module:      A3Types
Description: Types for Assignment 3
Copyright: (c) University of Toronto, 2023
               CSC324 Principles of Programming Languages, Fall 2023

This module provides the public types required for A3
You should review the data type Expr carefully, but do not
change anything in this file! We will use a fresh copy of this file
for testing purposes.
-}

module A3Types (Env, emptyEnv, Value(..), Expr(..)) where

import qualified Data.Map as Map

------------------------------------------------------------------------------
-- Data Definitions
------------------------------------------------------------------------------

-- | An environment is a mapping of names to values
type Env = Map.Map String Value
emptyEnv = Map.empty

-- | Like before, Values in Orange include:
data Value = T | F                -- booleans true and false
           | Num Int              -- integers
           | Empty                 -- the empty list
           | Pair Value Value     -- pairs
           | Closure ([Value] -> (Value -> Value) -> Value)
                                  -- closures are represented using a Haskell function,
                                  --    which take a list of arguments, a continuation,
                                  --    and calls the continuation with the output of the
                                  --    Orange application
           | Error String         -- errors    
          deriving (Eq, Show)

-- | We will represent Orange closures using Haskell functions.
--   To complete the CPS transform, this Haskell function also need
--   to be written in CPS.
instance Show ([Value] -> (Value -> Value) -> Value) where  
    show x = "[Can't Show Procedure]"
instance Eq ([Value] -> (Value -> Value) -> Value) where  
    x == y = False

-- | Expressions in Orange include:
data Expr = Literal Value           -- literal values
          | Plus Expr Expr          -- builtin "plus" function
          | Times Expr Expr         -- builtin "times" function
          | Equal Expr Expr         -- builtin checks for equality
          | Cons Expr Expr          -- builtin "cons" function that creates a pair
          | First Expr              -- builtin "first" function that obtains the first element of a pair
          | Rest Expr               -- builtin "rest" function that obtains the second element of a pair
          | If Expr Expr Expr       -- if statements
          | Var String              -- variable names
          | Lambda [String] Expr    -- function definitions
          | App Expr [Expr]         -- function applications
          | Shift String Expr       -- identical to `shift` in Racket
          | Reset Expr              -- identical to `reset` in Racket
          deriving (Eq, Show)

