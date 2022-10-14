
import Test.Tasty
import Test.Tasty.HUnit
import AST (pprint, AST(..), Operator(..))

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests =  testGroup "all" [ showTests, builder ]

showTests :: TestTree
showTests = testGroup "show" $ do
    (ast, x) <- [
        (Con 3, "3"),
        (Op PLUS (Con 2) (Con 2), "( 2 + 2 )"),
        (Op PLUS (Op PLUS (Con 2) (Con 3)) (Con 4), "( ( 2 + 3 ) + 4 )"),
        (Op PLUS (Con 1) (Op PLUS (Con 2) (Con 3)), "( 1 + ( 2 + 3 ) )")
       ] :: [(AST Int, String)]
    [testCase x $ pprint ast @?= x]

builder :: TestTree
builder = testGroup "builder" $ do
    (ast, x) <- [
        (Con 3, 3),
        (Op PLUS (Con 2) (Con 2), 2 + 2),
        (Op PLUS (Op PLUS (Con 2) (Con 3)) (Con 4), ( 2 + 3 ) + 4 ),
        (Op PLUS (Con 1) (Op PLUS (Con 2) (Con 3)), 1 + ( 2 + 3 ))
       ] :: [(AST Int, AST Int)]
    [testCase (pprint ast) $ ast @?= x]
