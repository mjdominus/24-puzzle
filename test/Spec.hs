import Test.Tasty
import qualified TestAST (tests)
import qualified TestEzpr (tests)

main :: IO ()
main = defaultMain $ testGroup "." [TestAST.tests, TestEzpr.tests]