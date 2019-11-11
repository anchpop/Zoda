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
import Optics
import qualified Data.Bifunctor as Data.Bifunctor
import Data.Void

data Module t p m i = Module (ModuleHeader t p m i) [(Declaration t p m i)] p deriving (Show, Eq, Generic, Typeable)
data ModuleHeader t p m i = ModuleHeader i (Tinydoc t p m i) p deriving (Show, Read, Eq, Ord, Generic, Typeable)

data Declaration t p m i = Declaration i (Expression t p m i) p deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal, Typeable)

data Expression t p m i = ParenthesizedExpression (Expression t p m i)                                     t p 
                        | FirstExpression (Expression t p m i)                                             t p 
                        | SecondExpression (Expression t p m i)                                            t p 
                        | PairExpression (Expression t p m i) (Expression t p m i)                         t p 
                        | TSigmaBinding (Expression t p m i) (Bind (i, (Atom, p)) (Expression t p m i))    t p 
                        | UniverseExpression Integer                                                       t p 
                        | NumberLiteral Integer Integer                                                    t p 
                        | AddExpression (Expression t p m i) (Expression t p m i)                          t p 
                        | ReferenceVariable i m                                                            t p 
                        | LambdaVariable (i, Atom)                                                         t p 
                        | FunctionLiteralExpression (Bind [(i, (Atom, p))] (Expression t p m i))           t p 
                        | FunctionApplicationExpression (Expression t p m i) [Expression t p m i]          t p 
                        | TArrowNonbinding (Expression t p m i)                    (Expression t p m i)    t p 
                        | TArrowBinding    (Expression t p m i) (Bind (i, (Atom, p)) (Expression t p m i)) t p
                        | Annotation (Expression t p m i) (Expression t p m i)                             t p 
                        | NatTypeExpression                                                                t p 
                        deriving (Show, Eq, Typeable, NominalSupport, NominalShow, Generic, Nominal)


data Tinydoc t p m i = Tinydoc Text p deriving (Show, Read, Eq, Ord, Generic, Typeable)
--data Identifier t p i = Identifier { getIdentifier :: i, getIdentifierSourcePos :: p } deriving (Show, Read, Eq, Ord, NominalSupport, NominalShow, Generic, Nominal, Typeable)


data Untyped = Untyped deriving (Show, Read, Eq, Ord, NominalSupport, NominalShow, Generic, Nominal)

instance Representational (Tinydoc t p) where rep Coercion = Coercion
instance Representational (Expression t p) where rep Coercion = Coercion
instance Representational (Declaration t p) where rep Coercion = Coercion
instance Representational (ModuleHeader t p) where rep Coercion = Coercion
instance Representational (Module t p) where rep Coercion = Coercion

type JustifiedModule     t p ph i = Map.Map ph i (Expression t p (Map.Key ph i) i)
type JustifiedExpression t p ph i = Expression t p (Map.Key ph i) i
