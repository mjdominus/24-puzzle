{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Ezpr where

import Data.List (intercalate)
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MS

data Ezpr a = Con a | Sum (MultiSet (Ezpr a)) (MultiSet (Ezpr a)) | Mul (MultiSet (Ezpr a)) (MultiSet (Ezpr a))
  deriving (Show)

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