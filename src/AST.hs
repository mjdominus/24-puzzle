module AST where

data OP a = OP { opName :: String, opFunc :: a -> a -> a }
data AST a = Con a | Op (OP a) (AST a) (AST a)