{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Ezpr (Ezpr (..), toStr) where

import Data.List (delete, intercalate)
import Data.Ratio (denominator, numerator)

data Op = SUM | MUL deriving (Eq, Show)

data Ezpr a
  = Con a
  | Oper Op (Bags a)
  deriving (Show)

-- Pattern guard
isOp :: Op -> Ezpr a -> Bool
isOp o = \case
  (Oper o' _) | o == o' -> True
  _ -> False

type Bag a = [Ezpr a]
type Bags a = (Bag a, Bag a)

equalAsBags :: Eq a => [a] -> [a] -> Bool
equalAsBags [] [] = True
equalAsBags [] _ = False
equalAsBags (a : as) bs = elem a bs && equalAsBags as (delete a bs)

leftBag :: Ezpr a -> Bag a
leftBag = fst . decompose

rightBag :: Ezpr a -> Bag a
rightBag = snd . decompose

decompose :: Ezpr a -> Bags a
decompose (Oper _ bags) = bags
-- maybe use ifcxt here : https://hackage.haskell.org/package/ifcxt
decompose _ = error $ "Can't decompose Ezpr "

separatorFor :: Op -> String
separatorFor SUM = "-"
separatorFor MUL = "÷"

toStr :: Show a => Ezpr a -> String
toStr = \case
  Con a -> show a
  Oper o (lt, rt) -> compound o lt (separatorFor o) rt
 where
  compound op lt sep rt =
    intercalate " " [show op, "[", lts, sep, rts, "]"]
   where
    lts = intercalate " " $ map toStr lt
    rts = intercalate " " $ map toStr rt

instance Eq a => Eq (Ezpr a) where
  (==) (Con a) (Con b) = a == b
  (==) (Con _) _ = False
  (==) _ (Con _) = False
  (==) (Oper o1 (lt1, rt1)) (Oper o2 (lt2, rt2)) =
    o1 == o2 && lt1 `equalAsBags` lt2 && rt1 `equalAsBags` rt2

instance (Eq a, Num a) => Num (Ezpr a) where
  (+) = addEzprs
  (-) = subEzprs
  (*) = mulEzprs

  negate (Con c) = Con (negate c)
  negate (Oper SUM (lt, rt)) = Oper SUM (rt, lt)
  negate x = Oper SUM ([], [x])

  signum = undefined
  abs = undefined

  fromInteger = Con . fromInteger

instance (Eq a, Num a) => Fractional (Ezpr a) where
  (/) = divEzprs
  fromRational r = mkOper MUL ([intCon n], [intCon d])
   where
    n = numerator r
    d = denominator r
    intCon = Con . fromInteger

-- if the two bags (a, b) have any elements in common,
-- remove the common elements from both
-- for example ([1,2,3,4], [2, 4, 4, 6]) -> ([1,3], [4,6])
removeDups :: Eq a => ([a], [a]) -> ([a], [a])
removeDups ([], b2) = ([], b2)
removeDups ((b : bs), b2) =
  if elem b b2
    then removeDups (bs, (delete b b2))
    else putback b $ removeDups (bs, b2)
 where
  putback b (b1, b2) = (b : b1, b2)

arithLeft :: (Eq a, Num a) => Op -> Ezpr a -> Ezpr a -> Ezpr a
arithLeft o (Oper o1 (lt1, rt1)) (Oper o2 (lt2, rt2))
  | o == o1
    , o == o2 =
    Oper o (lt1 ++ lt2, rt1 ++ rt2)
arithLeft o (Oper o1 (lt1, rt1)) e2 | o == o1 = Oper o (e2 : lt1, rt1)
arithLeft o e1 (Oper o2 (lt2, rt2)) | o == o2 = Oper o (e1 : lt2, rt2)
arithLeft o e1 e2 = Oper o ([e1, e2], [])

arithRight :: (Eq a, Num a) => Op -> Ezpr a -> Ezpr a -> Ezpr a
arithRight o (Oper o1 (lt1, rt1)) (Oper o2 (lt2, rt2))
  | o == o1
    , o == o2 =
    Oper o (lt1 ++ rt2, rt1 ++ lt2)
arithRight o (Oper o1 (lt1, rt1)) e2 | o == o1 = Oper o (lt1, e2 : rt1)
arithRight o e1 (Oper o2 (lt2, rt2)) | o == o2 = negate $ arithRight o (Oper o2 (lt2, rt2)) e1
arithRight o e1 e2 = Oper o ([e1], [e2])

mkOper :: Eq a => Op -> ([Ezpr a], [Ezpr a]) -> Ezpr a
mkOper op = Oper op . removeDups

addEzprs :: (Eq a, Num a) => Ezpr a -> Ezpr a -> Ezpr a
addEzprs e (Con 0) = e
addEzprs (Con 0) e = e
Oper SUM (lt1, rt1) `addEzprs` Oper SUM (lt2, rt2) =
  mkOper SUM (lt1 ++ lt2, rt1 ++ rt2)
Oper SUM (lt1, rt1) `addEzprs` e2 = mkOper SUM (e2 : lt1, rt1)
e1 `addEzprs` Oper SUM (lt2, rt2) = mkOper SUM (e1 : lt2, rt2)
e1 `addEzprs` e2 = mkOper SUM ([e1, e2], [])

subEzprs :: (Eq a, Num a) => Ezpr a -> Ezpr a -> Ezpr a
subEzprs e (Con 0) = e
subEzprs (Con 0) e = negate e
Oper SUM (lt1, rt1) `subEzprs` Oper SUM (lt2, rt2) =
  mkOper SUM (lt1 ++ rt2, rt1 ++ lt2)
Oper SUM (lt1, rt1) `subEzprs` e2 = mkOper SUM (lt1, e2 : rt1)
e1 `subEzprs` Oper SUM (lt2, rt2) = mkOper SUM (lt2, e1 : rt2)
e1 `subEzprs` e2 = mkOper SUM ([e1], [e2])

mulEzprs :: (Eq a, Num a) => Ezpr a -> Ezpr a -> Ezpr a
mulEzprs (Con 0) _ = Con 0
mulEzprs _ (Con 0) = Con 0
mulEzprs (Con 1) e = e
mulEzprs e (Con 1) = e
mulEzprs (Con (-1)) e = negate e
mulEzprs e (Con (-1)) = negate e
Oper MUL (lt1, rt1) `mulEzprs` Oper MUL (lt2, rt2) =
  mkOper MUL (lt1 ++ lt2, rt1 ++ rt2)
Oper MUL (lt1, rt1) `mulEzprs` e2 = mkOper MUL (e2 : lt1, rt1)
e1 `mulEzprs` Oper MUL (lt2, rt2) = mkOper MUL (e1 : lt2, rt2)
e1 `mulEzprs` e2 = mkOper MUL ([e1, e2], [])

divEzprs :: (Eq a, Num a) => Ezpr a -> Ezpr a -> Ezpr a
divEzprs e (Con 1) = e
divEzprs e (Con (-1)) = negate e
Oper MUL (lt1, rt1) `divEzprs` Oper MUL (lt2, rt2) =
  mkOper MUL (lt1 ++ lt2, rt1 ++ rt2)
Oper MUL (lt1, rt1) `divEzprs` e2 = mkOper MUL (e2 : lt1, rt1)
e1 `divEzprs` Oper MUL (lt2, rt2) = mkOper MUL (e1 : lt2, rt2)
e1 `divEzprs` e2 = mkOper MUL ([e1, e2], [])

example :: (Eq a, Num a) => Ezpr a
example = (3 + 4) + (5 + 6)

example2 :: (Eq a, Num a) => Ezpr a
example2 = (1 + (2 + 3)) + (4 + (5 + 6))

example3 :: Ezpr Int
example3 = (3 + 4) * (5 + 6) :: Ezpr Int

-- alex suggests for later:
--  smart constructors with pattern synonyms

{-
liftNodes :: Ezpr a -> Ezpr a
liftNodes (Sum add sub) =
  sum adds undefined
 where
  adds = sequence $ map added addSum
  -- addSum is the sum nodes in the addends of the current node
  -- addSum' is the non-sum nodes
  (addSum, addSum') = splitP isSum add
  (subSum, subSum') = splitP isSum sub
liftNodes x = x
 -}

-- SUM [ a b - c d ]     (a + b - c - d)

-- (1+2)+3   ->  SUM [ SUM [1 2 -] 3 - ]
--           ->  SUM [ 1 2 3 - ]

-- 1+(2+3)   ->  SUM [ 1 SUM [ 2 3 - ] - ]
--           ->  SUM [ 1 2 3 - ]

-- (1 - 2) - (3 - 4)
--   SUM [ SUM [ 1 - 2 ] - SUM [ 3 - 4 ] ]
--   SUM [ 1 - 2 SUM [ 3 - 4 ] ]      1 - 2 - (3-4)
--   SUM [ 1 4 - 2 3 ]                1 + 4 - 2 - 3
