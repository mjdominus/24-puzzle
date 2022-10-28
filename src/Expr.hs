module Expr (Expr (..), Op (..)) where

data Op = P | M | T | D

data Expr a = XCon a | Expr Op (Expr a) (Expr a)

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