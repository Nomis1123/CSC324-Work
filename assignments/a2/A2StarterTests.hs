{-|
Module: A2StarterTests
Description: Starter Tests for A2
Copyright: (c) University of Toronto Mississagua
               CSC324 Principles of Programming Languages, Fall 2025
-}

module A2StarterTest

where

import Test.QuickCheck (Property, (==>), label, quickCheck)

import A2 (run, eval)
import A2Types(Expr(..), Value(..), Env)

--Some simple tests to get you started--

prop_testLiteralNumber:: Int -> Property
prop_testLiteralNumber x = label "literal numbers" $
    let expr = (Literal $ Num x)
        result = run expr
    in result == Num x

prop_testAddition:: Int -> Int -> Property
prop_testAddition x y = label "addition tests" $
    let expr = (Plus (Literal $ Num x) (Literal $ Num y))
        result = run expr
    in result == Num (x + y)

prop_testBasicIdentifier :: Property
prop_testBasicIdentifier = label "identifier error" $
  let expr = (Plus (Literal $ Num 3) (Times (Literal $ Num 3) (Literal T)))
      result = run expr
  in result == Error "Times"

prop_testFunctionApplication :: Int -> Int -> Property
prop_testFunctionApplication x y = label "function application" $
    let fnExpr1 = Lambda ["x"] (Plus (Literal (Num 1)) (Var "x"))
        fnExpr2 = Lambda ["a", "b"] (Times (Var "a") (App fnExpr1 [(Var "b")]))
        result = run (App fnExpr2 [(Literal (Num x)), (Literal (Num y))])
    in result == Num (x * (y + 1))


-------------------------------------------------------------------------------
-- * Main function (for testing purposes only)
-------------------------------------------------------------------------------

-- This main function is executed when you compile and run this Haskell file.
-- It runs the QuickCheck tests; we'll talk about "do" notation much later in
-- the course, but for now if you want to add your own tests, just define them
-- above, and add a new `quickCheck` line below.
main :: IO ()
main = do
    quickCheck prop_testLiteralNumber
    quickCheck prop_testAddition
    quickCheck prop_testBasicIdentifier
    quickCheck prop_testFunctionApplication
