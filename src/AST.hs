{-# LANGUAGE DeriveFunctor #-}

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

data AST a = Con a | Op Operator (AST a) (AST a)
  deriving (Eq, Show, Functor)

pprint :: Show a => AST a -> String
pprint (Con a) = show a
pprint (Op op lt rt) = intercalate " " ["(", pprint lt, opName op, pprint rt, ")"]

instance Num a => Num (AST a) where
    (+) = Op PLUS
    (-) = Op MINUS
    (*) = Op TIMES
    fromInteger = Con . fromInteger
    abs = error "abs(AST) not defined"
    signum = error "signum(AST) not defined"

instance Fractional a => Fractional (AST a) where
    (/) = Op DIVIDE
    fromRational = Con . fromRational

-- Foldable
evalAST :: (Integral a, Fractional b) => AST a -> b
evalAST (Con a) = fromIntegral a
evalAST (Op op lt rt) = applyOp op (evalAST lt) (evalAST rt)
