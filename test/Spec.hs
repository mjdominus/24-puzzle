import Test.Tasty
import qualified TestAST (tests)

main :: IO ()
main = defaultMain TestAST.tests