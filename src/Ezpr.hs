{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Ezpr () where

import Data.List (intercalate)
import Data.Maybe (fromJust, isJust)
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MS

class Insertable f where
  insert :: a -> f a -> f a

-- create multiset version of list operator

ms :: ([a] -> c) -> MultiSet a -> c
ms f = MS.fromList . f . MS.toList

data Ezpr a
  = Con a
  | Sum (MultiSet (Ezpr a)) (MultiSet (Ezpr a))
  | Mul (MultiSet (Ezpr a)) (MultiSet (Ezpr a))
  deriving (Show)

decomposeSum :: Ezpr a -> Maybe (MultiSet (Ezpr a), MultiSet (Ezpr a))
decomposeSum (Sum ad sb) = Just (ad, sb)
decomposeSum (Mul _ _) = Nothing
decomposeSum (Con _) = Nothing

decomposeSum' :: Ezpr a -> (MultiSet (Ezpr a), MultiSet (Ezpr a))
decomposeSum' = fromJust . decomposeSum

decomposeMul :: Ezpr a -> Maybe (MultiSet (Ezpr a), MultiSet (Ezpr a))
decomposeMul (Sum _ _) = Nothing
decomposeMul (Mul nm dn) = Just (nm, dn)
decomposeMul (Con _) = Nothing

decomposeMul' :: Ezpr a -> (MultiSet (Ezpr a), MultiSet (Ezpr a))
decomposeMul' = fromJust . decomposeMul

isSum :: Ezpr a -> Bool
added :: Ezpr a -> MultiSet (Ezpr a)
subtracted :: Ezpr a -> MultiSet (Ezpr a)
isSum = isJust . decomposeSum
added = fst . fromJust . decomposeSum
subtracted = snd . fromJust . decomposeSum

isMul :: Ezpr a -> Bool
numerators :: Ezpr a -> MultiSet (Ezpr a)
denominators :: Ezpr a -> MultiSet (Ezpr a)
isMul = isJust . decomposeMul
numerators = fst . fromJust . decomposeMul
denominators = snd . fromJust . decomposeMul

toStr :: Show a => Ezpr a -> String
toStr = \case
  Con a -> show a
  Sum lt rt -> compound "SUM" lt "-" rt
  Mul lt rt -> compound "MUL" lt "÷" rt
 where
  compound op lt sep rt =
    intercalate " " [op, "[", lts, sep, rts, "]"]
   where
    lts = intercalate ", " $ map toStr $ MS.toList lt
    rts = intercalate ", " $ map toStr $ MS.toList rt

-- given a predicate p and a collection X,
-- compute two lists, one containing the elements of x
-- for which p is true, the other for which it is false
splitP :: (Foldable f) => (a -> Bool) -> f a -> ([a], [a])
splitP p = foldr f ([], [])
 where
  f x (yes, no) =
    if p x
      then (x : yes, no)
      else (yes, x : no)

-- take a SUM [b - c] SUM[d - e] and produce SUM [a b d - c e]

liftSums :: Ezpr a -> Ezpr a
liftSums (Sum lt rt) =
  Sum lts rts
 where
  lts = leftLeft <> rightRight <> nonSumsFromLeft
  rts = leftRight <> rightLeft <> nonSumsFromRight
  (sumsFromLeft, nonSumsFromLeft) = MS.partition isSum lt
  (leftLeft, leftRight) = unzip $ (fmap decomposeSum' sumsFromLeft :: _)
liftSums x = x

-- SUM $ MultiSet(1, 1, SUM(MultiSet(2, 3)]) ->  SUM (MultiSet (1 1 2 3))

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
