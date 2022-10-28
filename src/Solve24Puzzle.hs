module Solve24Puzzle (solve, solve24) where

import Expr (Expr, evalExpr, mkCon)

-- import ExprToEzpr ()

solve24 :: [Rational] -> [Expr Rational]
solve24 = solve 24

solve :: Rational -> [Rational] -> [Expr Rational]
solve target pool = filter ((== target) . evalExpr) (allExpressions (map mkCon pool))

allExpressions :: Fractional a => [a] -> [a]
allExpressions [] = []
allExpressions [e] = [e]
allExpressions (e1 : es) = do
    e2 <- allExpressions es
    [e1 + e2, e1 - e2, e2 - e1, e1 * e2, e1 / e2, e2 / e1]