
import Test.Tasty
import Test.Tasty.HUnit
import AST (pprint, AST(..), opPlus)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "show" $ do
    (ast, x) <- [
        (Con 3, "3"),
        (Op opPlus (Con 2) (Con 2), "( 2 + 2 )"),
        (Op opPlus (Op opPlus (Con 2) (Con 3)) (Con 4), "( ( 2 + 3 ) + 4 )"),
        (Op opPlus (Con 1) (Op opPlus (Con 2) (Con 3)), "( 1 + ( 2 + 3 ) )")
       ] :: [(AST Int, String)]
    [testCase x $ pprint ast @?= x]
