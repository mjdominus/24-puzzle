{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Ezpr () where

import Data.List (intercalate, partition)
import Data.Maybe (fromJust, isJust)

data Ezpr a
  = Con a
  | Add [Ezpr a] [Ezpr a]
  | Mul [Ezpr a] [Ezpr a]
  deriving (Show)

type Bag a = [Ezpr a]
type Bags a = (Bag a, Bag a)

leftBag :: Ezpr a -> Bag a
leftBag = fst . decompose

rightBag :: Ezpr a -> Bag a
rightBag = snd . decompose

decompose :: Show (Ezpr a) => Ezpr a -> Bags a
decompose (Add a b) = (a, b)
decompose (Mul a b) = (a, b)
decompose x = error $ "Can't decompose Ezpr " ++ show x

decomposeAdd :: Ezpr a -> Maybe (Bags a)
decomposeAdd (Add ad sb) = Just (ad, sb)
decomposeAdd (Mul _ _) = Nothing
decomposeAdd (Con _) = Nothing

decomposeAdd' :: Ezpr a -> Bags a
decomposeAdd' = fromJust . decomposeAdd

decomposeMul :: Ezpr a -> Maybe (Bags a)
decomposeMul (Add _ _) = Nothing
decomposeMul (Mul nm dn) = Just (nm, dn)
decomposeMul (Con _) = Nothing

decomposeMul' :: Ezpr a -> Bags a
decomposeMul' = fromJust . decomposeMul

isAdd :: Ezpr a -> Bool
added :: Ezpr a -> Bag a
subtracted :: Ezpr a -> Bag a
isAdd = isJust . decomposeAdd
added = fst . fromJust . decomposeAdd
subtracted = snd . fromJust . decomposeAdd

isMul :: Ezpr a -> Bool
numerators :: Ezpr a -> Bag a
denominators :: Ezpr a -> Bag a
isMul = isJust . decomposeMul
numerators = fst . fromJust . decomposeMul
denominators = snd . fromJust . decomposeMul

toStr :: Show a => Ezpr a -> String
toStr = \case
  Con a -> show a
  Add lt rt -> compound "ADD" lt "-" rt
  Mul lt rt -> compound "MUL" lt "÷" rt
 where
  compound op lt sep rt =
    intercalate " " [op, "[", lts, sep, rts, "]"]
   where
    lts = intercalate " " $ map toStr lt
    rts = intercalate " " $ map toStr rt

-- take a SUM [b - c] SUM[d - e] and produce SUM [a b d - c e]
liftSumsOld :: Ezpr a -> Ezpr a
liftSumsOld (Add lt rt) =
  Add lts rts
 where
  lts = nonAddsFromLeft <> concat leftLeft <> concat rightRight
  rts = nonAddsFromRight <> concat leftRight <> concat rightLeft
  (addsFromLeft, nonAddsFromLeft) = partition isAdd lt
  (addsFromRight, nonAddsFromRight) = partition isAdd rt
  (leftLeft, leftRight) = unzip $ fmap decomposeAdd' addsFromLeft
  (rightLeft, rightRight) = unzip $ fmap decomposeAdd' addsFromRight
liftSumsOld x = x

-- liftNodes :: Ezpr a -> Ezpr a
liftNodes ::
  (Ezpr a -> Bool) ->
  ([Ezpr a] -> [Ezpr a] -> t) ->
  (Ezpr a -> ([Ezpr a], [Ezpr a])) ->
  Ezpr a ->
  t
liftNodes nodeType constructor destructor node =
  constructor lts rts
 where
  lts = nonAddsFromLeft <> concat leftLeft <> concat rightRight
  rts = nonAddsFromRight <> concat leftRight <> concat rightLeft
  (addsFromLeft, nonAddsFromLeft) = partition nodeType (leftBag node)
  (addsFromRight, nonAddsFromRight) = partition nodeType (rightBag node)
  (leftLeft, leftRight) = unzip $ fmap destructor addsFromLeft
  (rightLeft, rightRight) = unzip $ fmap destructor addsFromRight

liftAdds :: Ezpr a -> Ezpr a
liftAdds = liftNodes isAdd Add decomposeAdd'

liftMuls :: Ezpr a -> Ezpr a
liftMuls = liftNodes isMul Mul decomposeMul'

example :: Num a => Ezpr a
example =
  Add
    [Add [Con 3] [Con 4]]
    [Add [Con 5] [Con 6]]

example2 :: Num a => Ezpr a
example2 =
  Add
    [Con 1, Add [Con 2] [Con 3]]
    [Con 4, Add [Con 5] [Con 6]]

example3 :: Num a => Ezpr a
example3 =
  Mul
    [Add [Con 3] [Con 4]]
    [Mul [Con 5] [Con 6]]

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
