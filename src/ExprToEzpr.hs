module ExprToEzpr (toEzpr) where

import Expr
import Ezpr

toEzpr :: (Fractional a, Eq a) => Expr a -> Ezpr a
toEzpr (XCon c) = Con c
toEzpr (Expr P a b) = toEzpr a + toEzpr b
toEzpr (Expr M a b) = toEzpr a - toEzpr b
toEzpr (Expr T a b) = toEzpr a * toEzpr b
toEzpr (Expr D a b) = toEzpr a / toEzpr b
