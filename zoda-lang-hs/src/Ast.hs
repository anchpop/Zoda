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
import Data.List.NonEmpty
 
data Module t p m i = Module (ModuleHeader t p m i) [(Declaration t p m i)] p deriving (Show, Eq, Generic, Typeable)
data ModuleHeader t p m i = ModuleHeader i (Tinydoc t p m i) p deriving (Show, Read, Eq, Ord, Generic, Typeable)

data Declaration t p m i = Declaration i (Expression t p m i) p deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal, Typeable)

data Expression t p m i = ParenthesizedExpression       (Expression t p m i)                                            t p 
                        | FirstExpression               (Expression t p m i)                                            t p 
                        | SecondExpression              (Expression t p m i)                                            t p 
                        | PairExpression                (Expression t p m i)           (Expression t p m i)             t p 
                        | TSigmaBinding (Expression t p m i) (Bind (NoBind i, (Atom, NoBind p)) (Expression t p m i))   t p 
                        | UniverseExpression Integer                                                                    t p 
                        | NumberLiteral Rational                                                                        t p 
                        | AddExpression                 (Expression t p m i)           (Expression t p m i)             t p 
                        | ReferenceVariable i m                                                                         t p 
                        | LambdaVariable (i, Atom)                                                                      t p 
                        | FunctionLiteralExpression (Bind (NonEmpty ((NoBind i, (Atom, NoBind p)), NoBind (Maybe (Expression t p m i)))) (Expression t p m i)) t p 
                        | FunctionApplicationExpression (Expression t p m i) (NonEmpty (Expression t p m i))            t p 
                        | TArrowBinding                 (Telescope  t p m i)                                            t p
                        | Annotation                    (Expression t p m i)           (Expression t p m i)             t p 
                        | NatTypeExpression                                                                             t p 
                        deriving (Show, Eq, Typeable, NominalSupport, NominalShow, Generic, Nominal)

data Telescope t p m i = Scope (Bind (Maybe (NoBind i, (Atom, NoBind p)), NoBind (Expression t p m i)) (Telescope t p m i)) 
                       | Pi    (Bind (Maybe (NoBind i, (Atom, NoBind p)), NoBind (Expression t p m i)) (Expression t p m i)) deriving (Show, Eq, Typeable, NominalSupport, NominalShow, Generic, Nominal)

data Tinydoc t p m i = Tinydoc Text p deriving (Show, Read, Eq, Ord, Generic, Typeable)



data Untyped = Untyped deriving (Show, Read, Eq, Ord, NominalSupport, NominalShow, Generic, Nominal, Bindable)

instance Representational (Tinydoc t p) where rep Coercion = Coercion
instance Representational (Expression t p) where rep Coercion = Coercion
instance Representational (Declaration t p) where rep Coercion = Coercion
instance Representational (ModuleHeader t p) where rep Coercion = Coercion
instance Representational (Module t p) where rep Coercion = Coercion

type JustifiedModule     t p ph m i = Map.Map ph m (Expression t p (Map.Key ph m) i)
type JustifiedExpression t p ph m i = Expression t p (Map.Key ph m) i

