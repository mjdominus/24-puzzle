module Expr (Expr (..), Op (..), evalExpr, mkCon, toStr) where

import Data.List (intercalate)

data Op = P | M | T | D
    deriving (Eq, Show)

opSymbolFor :: Op -> String
opSymbolFor P = "+"
opSymbolFor M = "-"
opSymbolFor T = "*"
opSymbolFor D = "/"

data Expr a = XCon a | Expr Op (Expr a) (Expr a)
    deriving (Eq, Show)

mkCon :: a -> Expr a
mkCon = XCon

toStr :: Show a => Expr a -> String
toStr (XCon a) = show a
toStr (Expr o a b) = intercalate " " ["(", toStr a, opSymbolFor o, toStr b, ")"]

instance Num a => Num (Expr a) where
    (+) = Expr P
    (-) = Expr M
    (*) = Expr T
    fromInteger = XCon . fromInteger
    negate (XCon c) = XCon (negate c)
    negate e = Expr M e (XCon (-1))
    signum = undefined
    abs = undefined

instance Fractional a => Fractional (Expr a) where
    (/) = Expr D
    fromRational = XCon . fromRational
-- can I use this for ExprToEzpr?
evalExpr :: (Real a, Fractional b) => Expr a -> b
evalExpr (XCon c) = fromRational $ toRational c
evalExpr (Expr P a b) = evalExpr a + evalExpr b
evalExpr (Expr M a b) = evalExpr a - evalExpr b
evalExpr (Expr T a b) = evalExpr a * evalExpr b
evalExpr (Expr D a b) = evalExpr a / evalExpr b
