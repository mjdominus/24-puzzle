module ExprToEzpr (toEzpr) where

import Expr (Expr (..), Op (..))
import Ezpr

-- can I just use evalExpr here?
-- not without some work it seems
-- the sticking point seems to be: how to convert XCon c -> t ?
toEzpr :: (Num a, Eq a) => Expr a -> Ezpr a
-- toEzpr = evalExpr
toEzpr (XCon c) = Con c
toEzpr (Expr P a b) = toEzpr a + toEzpr b
toEzpr (Expr M a b) = toEzpr a - toEzpr b
toEzpr (Expr T a b) = toEzpr a * toEzpr b
toEzpr (Expr D a b) = toEzpr a / toEzpr b
