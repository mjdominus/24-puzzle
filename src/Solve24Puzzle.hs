module Solve24Puzzle (rawSolve, solve, solve24) where

import Expr (Expr, evalExpr, mkCon)
import ExprToEzpr (toEzpr)
import Ezpr (Ezpr)
import Data.List (delete, nub)

-- import ExprToEzpr ()

solve24 :: Real a => [a] -> [Ezpr a]
solve24 = solve (24 :: Rational)

-- solve :: Rational -> [Rational] -> [Expr Rational]
-- solve :: Rational -> [Rational] -> [Expr Rational]
solve :: (Real a, Fractional b, Eq b) => b -> [a] -> [Ezpr a]
solve target pool = nub $ map toEzpr $ rawSolve target pool

rawSolve :: (Real a, Fractional b, Eq b) => b -> [a] -> [Expr a]
rawSolve target pool = filter ((== target) . evalExpr) (allExpressions (map mkCon pool))

allExpressions :: (Eq a, Fractional a) => [a] -> [a]
allExpressions e = processQueue moreExpressions [e] []
  where
    moreExpressions exprs0 = do
        (a1, exprs1) <- splits exprs0
        (a2, exprs2) <- splits exprs1
        map (: exprs2)  [a1 + a2,
            a1 - a2, a2 - a1,
            a1 * a2,
            a1 / a2, a2 / a1]

splits :: Eq a => [a] -> [(a, [a])]
splits as = do
    a <- as
    return (a, delete a as)

      
processQueue :: ([a] -> [[a]]) -> [[a]] -> [a] -> [a]
processQueue _ [] out = out
processQueue f (a:as) out =
        case a of [] -> processQueue f as out
                  [e] -> processQueue f as (e:out)
                  _ -> processQueue f (f a ++ as) out


subsum :: (Eq a, Num a) => [a] -> [[a]]
subsum ns0 = do
    (n1, ns1) <- splits ns0 
    (n2, ns2)  <- splits ns1
    return $ (n1+n2) : ns2