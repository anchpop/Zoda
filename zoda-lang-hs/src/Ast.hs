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
data ModuleHeader t p i = ModuleHeader (Identifier t p i) (Tinydoc t p i) p deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Typeable)

data Declaration t p i = Declaration (Identifier t p i) (Expression t p i) p deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Typeable)

data Expression t p i = ParenthesizedExpression (Expression t p i) t p 
                      | NumberLiteral Rational t p 
                      | IdentifierExpression (Identifier t p i) t p 
                      | FunctionLiteralExpression [Identifier t p i] (Expression t p i) t p 
                      | FunctionApplicationExpression (Expression t p i) [Expression t p i] t p 
                      | TArrow (Maybe (Expression t p i)) (Expression t p i) (Expression t p i) t p 
                      | Annotation (Expression t p i) (Expression t p i) t p 
                        deriving (Show, Eq, Read, Ord, Functor, Foldable, Traversable, Typeable)


data Tinydoc t p i = Tinydoc Text p deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Typeable)
data Identifier t p i = Identifier { getIdentifier :: i, getIdentifierSourcePos :: p } deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Typeable)


data Untyped = Untyped deriving (Show, Read, Eq, Ord)

makeBaseFunctor ''Expression

instance Representational (Identifier t p) where rep Coercion = Coercion
instance Representational (Tinydoc t p) where rep Coercion = Coercion
instance Representational (Expression t p) where rep Coercion = Coercion
instance Representational (Declaration t p) where rep Coercion = Coercion
instance Representational (ModuleHeader t p) where rep Coercion = Coercion
instance Representational (Module t p) where rep Coercion = Coercion

