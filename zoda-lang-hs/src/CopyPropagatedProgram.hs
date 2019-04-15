module CopyPropagatedProgram where
import Data.Int
import Data.Ratio

data Expression p = DirectReference (Expression p) p | RationalNumber Rational p deriving (Show, Read, Eq, Ord)