module Ast where
import ClassyPrelude
import Data.Type.Coercion
import Data.Roles
import Data.Functor.Foldable
import Text.Show.Deriving
import Text.Read.Deriving
import Data.Eq.Deriving
import Data.Ord.Deriving
import Data.Functor.Foldable.TH
import qualified Data.Map.Justified as Map

data Module t p i = Module (ModuleHeader t p i) [(Declaration t p i)] p deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Typeable)
data ModuleHeader t p i = ModuleHeader (LowercaseIdentifier t p i) (Tinydoc t p i) p deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Typeable)

data Declaration t p i = Declaration (LowercaseIdentifier t p i) (Expression t p i) p deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Typeable)

data Expression t p i = ParenthesizedExpression (Expression t p i) t p 
                      | NumberLiteral Rational t p 
                      | IdentifierExpression (LowercaseIdentifier t p i) t p 
                      | FunctionLiteralExpression (FunctionLiteral t p i) t p 
                      | FunctionApplicationExpression (Expression t p i) [Expression t p i] t p deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Typeable)


data FunctionLiteral t p i = FunctionLiteral [LowercaseIdentifier t p i] (Expression t p i) p deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Typeable)


data Tinydoc t p i = Tinydoc Text p deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Typeable)
data LowercaseIdentifier t p i = LowercaseIdentifier i p deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Typeable)
data UppercaseIdentifier t p i = UppercaseIdentifier i p deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Typeable)



data Untyped = Untyped deriving (Show, Read, Eq, Ord)

makeBaseFunctor ''Expression

instance Representational (LowercaseIdentifier t p) where rep Coercion = Coercion
instance Representational (UppercaseIdentifier t p) where rep Coercion = Coercion
instance Representational (Tinydoc t p) where rep Coercion = Coercion
instance Representational (FunctionLiteral t p) where rep Coercion = Coercion
instance Representational (Expression t p) where rep Coercion = Coercion
instance Representational (Declaration t p) where rep Coercion = Coercion
instance Representational (ModuleHeader t p) where rep Coercion = Coercion
instance Representational (Module t p) where rep Coercion = Coercion

