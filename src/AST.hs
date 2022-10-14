module AST where
    
import Data.List (intercalate)

data Operator = PLUS | MINUS | TIMES | DIVIDE 
    deriving (Eq, Show)

applyOp :: Fractional a => Operator -> a -> a -> a
applyOp PLUS = (+)
applyOp MINUS = (-)
applyOp TIMES = (*)
applyOp DIVIDE = (/)

opName :: Operator -> String
opName PLUS = "+"
opName MINUS = "-"
opName TIMES = "×"
opName DIVIDE = "÷"

data (Eq a, Show a) => AST a = Con a | Op Operator (AST a) (AST a)
  deriving (Eq, Show)

pprint :: (Eq a, Show a) => AST a -> String
pprint (Con a) = show a
pprint (Op op lt rt) = intercalate " " ["(", pprint lt, opName op, pprint rt, ")"]

instance (Num a, Show a, Eq a) => Num (AST a) where
    (+) = Op PLUS
    (-) = Op MINUS
    (*) = Op TIMES
    fromInteger = Con . fromInteger
    abs = error "abs(AST) not defined"
    signum = error "signum(AST) not defined"


