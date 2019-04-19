module Ast where
import ClassyPrelude

data Module p i = Module (ModuleHeader p i) [(Declaration p i)] p deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Typeable)
data ModuleHeader p i = ModuleHeader (LowercaseIdentifier p i) (Tinydoc p i) p deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Typeable)

data Declaration p i = Declaration (LowercaseIdentifier p i) (Expression p i) p deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Typeable)

data Expression p i = ParenthesizedExpression (Expression p i) p 
                    | NumberLiteralExpression (NumberLiteral p i) p 
                    | IdentifierExpression (LowercaseIdentifier p i) p 
                    | FunctionLiteralExpression (FunctionLiteral p i) p 
                    | FunctionApplicationExpression (Expression p i) [Expression p i] p deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Typeable)



data NumberLiteral p i = NumberLiteral Rational p deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Typeable)
data FunctionLiteral p i = FunctionLiteral [LowercaseIdentifier p i] (Expression p i) p deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Typeable)



data Tinydoc p i = Tinydoc Text p deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Typeable)
data LowercaseIdentifier p i = LowercaseIdentifier i p deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Typeable)
data UppercaseIdentifier p i = UppercaseIdentifier i p deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Typeable)



