module TestAST (tests) where

import AST (AST (..), Operator (..), evalAST, pprint)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "AST" [showTests, builder, eval]

showTests :: TestTree
showTests = testGroup "pprint" $ do
  (ast, x) <-
    [ (Con 3, "3")
    , (Op PLUS (Con 2) (Con 2), "( 2 + 2 )")
    , (Op PLUS (Op PLUS (Con 2) (Con 3)) (Con 4), "( ( 2 + 3 ) + 4 )")
    , (Op PLUS (Con 1) (Op PLUS (Con 2) (Con 3)), "( 1 + ( 2 + 3 ) )")
    ] ::
      [(AST Int, String)]
  [testCase x $ pprint ast @?= x]

builder :: TestTree
builder = testGroup "builder" $ do
  (ast, x) <-
    [ (Con 3, 3)
    , (Op PLUS (Con 2) (Con 2), 2 + 2)
    , (Op PLUS (Op PLUS (Con 2) (Con 3)) (Con 4), (2 + 3) + 4)
    , (Op PLUS (Con 1) (Op PLUS (Con 2) (Con 3)), 1 + (2 + 3))
    ] ::
      [(AST Int, AST Int)]
  [testCase (pprint ast) $ ast @?= x]

eval :: TestTree
eval =
  testGroup
    "eval"
    [ testGroup "eval1" $ do
        (ast, x) <-
          [ (Con 3, 3.0)
          , (Op PLUS (Con 2) (Con 2), 4.0)
          , (Op PLUS (Op PLUS (Con 2) (Con 3)) (Con 4), 9.0)
          , (Op PLUS (Con 1) (Op PLUS (Con 2) (Con 3)), 6.0)
          ] ::
            [(AST Int, Float)]
        [testCase (pprint ast) $ evalAST ast @?= x]
    , testGroup "Int -> Float" $ do
        (ast, x) <-
          [ (Op DIVIDE (Con 3) (Con 2), 1.5)
          ] ::
            [(AST Int, Float)]
        [testCase (pprint ast) $ evalAST ast @?= x]
    ]