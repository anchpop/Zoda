module Ast where
import ClassyPrelude

data Module p = Module (ModuleHeader p) [(Declaration p)] p deriving (Show, Read, Eq, Ord)
data ModuleHeader p = ModuleHeader (LowercaseIdentifier p) (Tinydoc p) p deriving (Show, Read, Eq, Ord)

data Declaration p = Declaration (LowercaseIdentifier p) (Expression p) p deriving (Show, Read, Eq, Ord)

data Expression p = ParenthesizedExpression (Expression p) p | NumberLiteralExpression (NumberLiteral p) p | IdentifierExpression (LowercaseIdentifier p) p | FunctionLiteralExpression (FunctionLiteral p) p | FunctionApplicationExpression (Expression p) [Expression p] p deriving (Show, Read, Eq, Ord)

data NumberLiteral p = NumberLiteral Rational p deriving (Show, Read, Eq, Ord)
data FunctionLiteral p = FunctionLiteral [LowercaseIdentifier p] (Expression p) p deriving (Show, Read, Eq, Ord)



data Tinydoc p = Tinydoc Text p deriving (Show, Read, Eq, Ord)
data LowercaseIdentifier p = LowercaseIdentifier Text p deriving (Show, Read, Eq, Ord)
data UppercaseIdentifier p = UppercaseIdentifier Text p deriving (Show, Read, Eq, Ord)
