module AST where
    
import Data.List (intercalate)

data Operator a = Operator { opName :: String, opFunc :: a -> a -> a }

opPlus :: Num a => Operator a
opPlus = Operator { opName="+", opFunc=(+) }


instance Show (Operator a) where
    show (Operator {opName=n}) = "Op(" ++ n ++ ")"

instance Eq (Operator a) where
    o1 == o2   = opName o1 == opName o2

data (Num a, Show a) => AST a = Con a | Op (Operator a) (AST a) (AST a)
  deriving (Eq, Show)

pprint :: (Num a, Show a) => AST a -> String
pprint (Con a) = show a
pprint (Op op lt rt) = intercalate " " ["(", pprint lt, opName op, pprint rt, ")"]



