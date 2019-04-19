module Ast where
import ClassyPrelude

data Module i p = Module (ModuleHeader i p) [(Declaration i p)] p deriving (Show, Read, Eq, Ord)
data ModuleHeader i p = ModuleHeader (LowercaseIdentifier i p) (Tinydoc i p) p deriving (Show, Read, Eq, Ord)

data Declaration i p = Declaration (LowercaseIdentifier i p) (Expression i p) p deriving (Show, Read, Eq, Ord)

data Expression i p = ParenthesizedExpression (Expression i p) p 
                    | NumberLiteralExpression (NumberLiteral i p) p 
                    | IdentifierExpression (LowercaseIdentifier i p) p 
                    | FunctionLiteralExpression (FunctionLiteral i p) p 
                    | FunctionApplicationExpression (Expression i p) [Expression i p] p deriving (Show, Read, Eq, Ord)

data NumberLiteral i p = NumberLiteral Rational p deriving (Show, Read, Eq, Ord)
data FunctionLiteral i p = FunctionLiteral [LowercaseIdentifier i p] (Expression i p) p deriving (Show, Read, Eq, Ord)



data Tinydoc i p = Tinydoc Text p deriving (Show, Read, Eq, Ord)
data LowercaseIdentifier i p = LowercaseIdentifier i p deriving (Show, Read, Eq, Ord)
data UppercaseIdentifier i p = UppercaseIdentifier i p deriving (Show, Read, Eq, Ord)
