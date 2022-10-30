module Main (main) where

import Data.List (nub)
import qualified Expr (toStr)
import ExprToEzpr (toEzpr)
import qualified Ezpr (Ezpr, toStr)
import Solve24Puzzle (rawSolve, solve)

main :: IO ()
main = do
    let s = rawSolve 24 [1, 2, 7, 7]
    putStrLn $ "Found " ++ show (length s) ++ " solutions"
    let ss = map (\x -> (Expr.toStr x ++ " -> " ++ (Ezpr.toStr $ toEzpr x))) s
    putStrLn $ unlines ss
    let sols = nub $ map toEzpr s
    putStrLn $ "No, actually only " ++ show (length sols) ++ " solutions"
    putStrLn $ unlines (map Ezpr.toStr sols)