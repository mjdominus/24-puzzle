module TestEzpr (tests) where

import Ezpr ()
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testCase "dummy" $ do
    2 + 2 @?= (5 :: Int)