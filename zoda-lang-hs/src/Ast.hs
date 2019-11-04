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
import Nominal hiding ((.))

data Module t p m i = Module (ModuleHeader t p m i) [(Declaration t p m i)] p deriving (Show, Eq, Generic, Typeable)
data ModuleHeader t p m i = ModuleHeader i (Tinydoc t p m i) p deriving (Show, Read, Eq, Ord, Generic, Typeable)

data Declaration t p m i = Declaration i (Expression t p m i) p deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal, Typeable)

data Expression t p m i = ParenthesizedExpression (Expression t p m i) t p 
                        | NumberLiteral Integer Integer t p 
                        | ReferenceVariable i m t p 
                        | LambdaVariable Atom t p 
                        | FunctionLiteralExpression (Bind [(Atom, i, p)] (Expression t p m i)) t p 
                        | FunctionApplicationExpression (Expression t p m i) [Expression t p m i] t p 
                        | TArrow (Maybe (Expression t p m i)) (Expression t p m i) (Expression t p m i) t p 
                        | Annotation (Expression t p m i) (Expression t p m i) t p 
                        deriving (Show, Eq, Typeable, NominalSupport, NominalShow, Generic, Nominal)




data Tinydoc t p m i = Tinydoc Text p deriving (Show, Read, Eq, Ord, Generic, Typeable)
--data Identifier t p i = Identifier { getIdentifier :: i, getIdentifierSourcePos :: p } deriving (Show, Read, Eq, Ord, NominalSupport, NominalShow, Generic, Nominal, Typeable)


data Untyped = Untyped deriving (Show, Read, Eq, Ord, NominalSupport, NominalShow, Generic, Nominal)

instance Representational (Tinydoc t p) where rep Coercion = Coercion
instance Representational (Expression t p) where rep Coercion = Coercion
instance Representational (Declaration t p) where rep Coercion = Coercion
instance Representational (ModuleHeader t p) where rep Coercion = Coercion
instance Representational (Module t p) where rep Coercion = Coercion

