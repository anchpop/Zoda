module Ast where
import ClassyPrelude
import Data.Type.Coercion
import Data.Roles
import Data.Functor.Foldable
import Text.Show.Deriving
import Text.Read.Deriving
import Data.Eq.Deriving
import Data.Ord.Deriving
import qualified Data.Map.Justified as Map
import Nominal hiding ((.))
import Optics
import qualified Data.Bifunctor as Data.Bifunctor
import Data.Void
import Data.List.NonEmpty
 
data Module t p m i = Module (ModuleHeader t p m i) [(Declaration t p m i)] p deriving (Show, Eq, Generic, Typeable)
data ModuleHeader t p m i = ModuleHeader i (Tinydoc t p m i) p deriving (Show, Read, Eq, Ord, Generic, Typeable)

data Declaration t p m i = Declaration i (Expression t p m i) p deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal, Typeable)

data Expression t p m i = ParenthesizedExpression       (Expression t p m i)                                                          t p 
                        | FirstExpression               (Expression t p m i)                                                          t p 
                        | SecondExpression              (Expression t p m i)                                                          t p 
                        | PairExpression                (Expression t p m i)                                    (Expression t p m i)  t p 
                        | TSigmaBinding                 (Expression t p m i) (Bind (Maybe (Atom, NoBind i, NoBind p)) (Expression t p m i)) t p 
                        | UniverseExpression Integer                                                                                  t p 
                        | NumberLiteral Rational                                                                                      t p 
                        | AddExpression                 (Expression t p m i)                                    (Expression t p m i)  t p 
                        | ReferenceVariable i m                                                                                       t p 
                        | LambdaVariable (Atom, i)                                                                                    t p 
                        | FunctionLiteralExpression     (FunctionLiteral t p m i)                                                     t p 
                        | FunctionApplicationExpression (Expression t p m i)                          (NonEmpty (Expression t p m i)) t p 
                        | TArrowBinding                 (Telescope  t p m i)                                                          t p
                        | Annotation                    (Expression t p m i)                                    (Expression t p m i)  t p 
                        | NatTypeExpression                                                                                           t p 
                        deriving (Show, Eq, Typeable, NominalSupport, NominalShow, Generic, Nominal)

data Telescope t p m i = Scope (Expression t p m i) (Bind (Maybe (Atom, NoBind i, NoBind p)) (Telescope t p m i)) 
                       | Pi    (Expression t p m i) (Bind (Maybe (Atom, NoBind i, NoBind p)) (Expression t p m i)) deriving (Show, Eq, Typeable, NominalSupport, NominalShow, Generic, Nominal)

data FunctionLiteral t p m i = Arg     (Expression t p m i) (Bind (Atom, NoBind i, NoBind p) (FunctionLiteral t p m i)) 
                             | LastArg (Expression t p m i) (Bind (Atom, NoBind i, NoBind p) (Expression t p m i)) deriving (Show, Eq, Typeable, NominalSupport, NominalShow, Generic, Nominal)


data Tinydoc t p m i = Tinydoc Text p deriving (Show, Read, Eq, Ord, Generic, Typeable)

data Untyped = Untyped deriving (Show, Read, Eq, Ord, NominalSupport, NominalShow, Generic, Nominal, Bindable)

instance Representational (Tinydoc t p) where rep Coercion = Coercion
instance Representational (Expression t p) where rep Coercion = Coercion
instance Representational (Declaration t p) where rep Coercion = Coercion
instance Representational (ModuleHeader t p) where rep Coercion = Coercion
instance Representational (Module t p) where rep Coercion = Coercion

type JustifiedModule     t p ph m i = Map.Map ph m (Expression t p (Map.Key ph m) i)
type JustifiedExpression t p ph m i = Expression t p (Map.Key ph m) i
type JustifiedTelescope  t p ph m i = Telescope t p (Map.Key ph m) i
type JustifiedFunctionLiteral  t p ph m i = FunctionLiteral t p (Map.Key ph m) i


getOutputOfScope :: (Nominal t, Nominal p, Nominal m, Nominal i) => Telescope t p m i -> Expression t p m i 
getOutputOfScope (Scope _ (_ :. scope) ) = getOutputOfScope scope
getOutputOfScope (Pi    _ (_ :. e) ) = e

getBodyOfFlit :: (Nominal t, Nominal p, Nominal m, Nominal i) => FunctionLiteral t p m i -> Expression t p m i 
getBodyOfFlit (Arg     _ (_ :. scope)) = getBodyOfFlit scope
getBodyOfFlit (LastArg _ (_ :. e)) = e





substExpr lookingFor substWith = subst 
  where subst (ParenthesizedExpression e t p) = ParenthesizedExpression (subst e) t p
        subst (FirstExpression e t p) = FirstExpression (subst e) t p
        subst (SecondExpression e t p) = SecondExpression (subst e) t p
        subst (PairExpression e1 e2 t p) = PairExpression (subst e1) (subst e2) t p
        subst (TSigmaBinding e1 (a :. e2) t p) = TSigmaBinding (subst e1) (a :. (subst e2)) t p
        subst (UniverseExpression i t p) = UniverseExpression i t p
        subst (NumberLiteral i t p) = NumberLiteral i t p
        subst (AddExpression e1 e2 t p) = AddExpression (subst e1) (subst e2) t p
        subst (ReferenceVariable i m t p) = ReferenceVariable i m t p
        subst (LambdaVariable (a, i) t p) = if a == lookingFor then substWith else LambdaVariable (a, i) t p
        subst (FunctionApplicationExpression func args t p) = FunctionApplicationExpression (subst func) (fmap subst args) t p
        subst (TArrowBinding scope t p) = TArrowBinding (substTelescope lookingFor substWith scope) t p
        subst (Annotation e1 e2 t p) = Annotation (subst e1) (subst e2) t p
        subst (NatTypeExpression t p) = NatTypeExpression t p
        subst (FunctionLiteralExpression flit t p) = FunctionLiteralExpression (substFlit lookingFor substWith flit) t p

substTelescope lookingFor substWith = subst 
  where substE = substExpr lookingFor substWith
        subst (Scope e1 (a :. scope)) = Scope (substE e1) (a :. subst scope)
        subst (Pi    e1 (a :. e2   )) = Pi (substE e1) (a :. substE e2)
substFlit lookingFor substWith = subst
  where substE = substExpr lookingFor substWith
        subst (Arg     e1 (a :. scope)) = Arg     (substE e1) (a :. subst scope)
        subst (LastArg e1 (a :. e2   )) = LastArg (substE e1) (a :. substE e2)


liftA4 f a b c d = liftA3 f a b c <*> d

traverseExpr :: forall t1 p1 m1 i1 t2 p2 m2 i2 a. (Bindable t1, Bindable t2, Bindable p1, Bindable p2, Bindable i1, Bindable i2, Bindable m1, Bindable m2, Applicative a) => (t1 -> a t2) -> (p1 -> a p2) -> (m1 -> a m2) ->  (i1 -> a i2) -> Expression t1 p1 m1 i1 -> a (Expression t2 p2 m2 i2)
traverseExpr ft fp fm fi = me
  where me :: Expression t1 p1 m1 i1 -> a (Expression t2 p2 m2 i2)
        me (ParenthesizedExpression e t p) = liftA3 ParenthesizedExpression (me e) (ft t) (fp p)
        me (FirstExpression e t p)         = (liftA3 FirstExpression) (me e) (ft t) (fp p)
        me (SecondExpression e t p)        = (liftA3 SecondExpression) (me e) (ft t) (fp p)
        me (PairExpression e1 e2 t p)      = (liftA4 PairExpression) (me e1) (me e2) (ft t) (fp p)
        me (TSigmaBinding e1 (Just (a, NoBind i, NoBind p1) :. e2) t p2) = liftA4 TSigmaBinding (me e1) (liftA2 abst (liftA2 (\x y -> Just (a, NoBind x, NoBind y)) (fi i) (fp p1)) (me e2)) (ft t) (fp p2)
        me (TSigmaBinding e1 (Nothing                       :. e2) t p2) = liftA4 TSigmaBinding (me e1) (liftA2 abst (pure Nothing) (me e2)) (ft t) (fp p2)
        me (UniverseExpression i t p)      = (liftA2 (UniverseExpression i)) (ft t) (fp p)
        me (NumberLiteral i t p)           = (liftA2 (NumberLiteral i)) (ft t) (fp p)
        me (AddExpression e1 e2 t p)       = (liftA4 AddExpression) (me e1) (me e2) (ft t) (fp p)
        me (ReferenceVariable i m t p)     = (liftA4 ReferenceVariable) (fi i) (fm m) (ft t) (fp p)
        me (LambdaVariable (a, i) t p)     = (liftA3 LambdaVariable) ((\x -> (a, x)) <$> fi i) (ft t) (fp p)
        me (FunctionLiteralExpression flit t p) = (liftA3 FunctionLiteralExpression) (traverseFunctionLiteral ft fp fm fi flit) (ft t) (fp p)
        me (FunctionApplicationExpression func args t p) = liftA4 FunctionApplicationExpression (me func) (traverse me args) (ft t) (fp p)
        me (TArrowBinding telescope t p) = (liftA3 TArrowBinding) (traverseTelescope ft fp fm fi telescope) (ft t) (fp p)
        me (Annotation e1 e2 t p) = (liftA4 Annotation) (me e1) (me e2) (ft t) (fp p)
        me (NatTypeExpression t p) = (liftA2 NatTypeExpression) (ft t) (fp p)

traverseTelescope :: forall t1 p1 m1 i1 t2 p2 m2 i2 a. (Bindable t1, Bindable t2, Bindable p1, Bindable p2, Bindable i1, Bindable i2, Bindable m1, Bindable m2, Applicative a) => (t1 -> a t2) -> (p1 -> a p2) -> (m1 -> a m2) ->  (i1 -> a i2) -> Telescope t1 p1 m1 i1 -> a (Telescope t2 p2 m2 i2)
traverseTelescope ft fp fm fi = te
  where te (Scope e ((Just (a, NoBind i, NoBind p)) :. scope))   = liftA2 Scope (me e) (liftA2 abst (liftA3 filler (pure a) (fi i) (fp p)) (te scope))
        te (Scope e ((Nothing                       :. scope)))  = liftA2 Scope (me e) (liftA2 abst (pure Nothing)                         (te scope))
        te (Pi    e ((Just (a, NoBind i, NoBind p)) :. eBound))  = liftA2 Pi    (me e) (liftA2 abst (liftA3 filler (pure a) (fi i) (fp p)) (me eBound))
        te (Pi    e ((Nothing                       :. eBound))) = liftA2 Pi    (me e) (liftA2 abst (pure Nothing)                         (me eBound))
        filler a i p = Just (a, NoBind i, NoBind p)
        me = traverseExpr ft fp fm fi


traverseFunctionLiteral :: forall t1 p1 m1 i1 t2 p2 m2 i2 a. (Bindable t1, Bindable t2, Bindable p1, Bindable p2, Bindable i1, Bindable i2, Bindable m1, Bindable m2, Applicative a) => (t1 -> a t2) -> (p1 -> a p2) -> (m1 -> a m2) ->  (i1 -> a i2) -> FunctionLiteral t1 p1 m1 i1 -> a (FunctionLiteral t2 p2 m2 i2)
traverseFunctionLiteral ft fp fm fi = fe
  where fe (Arg     e (((a, NoBind i, NoBind p)) :. scope))   = liftA2 Arg     (me e) (liftA2 abst (liftA3 filler (pure a) (fi i) (fp p)) (fe scope))
        fe (LastArg e (((a, NoBind i, NoBind p)) :. eBound))  = liftA2 LastArg (me e) (liftA2 abst (liftA3 filler (pure a) (fi i) (fp p)) (me eBound))
        filler a i p = (a, NoBind i, NoBind p)
        me = traverseExpr ft fp fm fi

traverseExpr2 :: (Bindable t, Bindable p, Bindable i, Bindable m1, Bindable m2, Monad mo) => (m1 -> mo m2) -> Expression t p m1 i -> mo (Expression t p m2 i)
traverseExpr2 fm = traverseExpr (fmap pure id) (fmap pure id) (fm) (fmap pure id)

forExpr2 :: (Bindable t, Bindable p, Bindable i, Monad mo, Bindable m1, Bindable m2) => Expression t p m1 i -> (m1 -> mo m2) -> mo (Expression t p m2 i)
forExpr2 x y = traverseExpr2 y x


mapExpr :: forall t1 p1 m1 i1 t2 p2 m2 i2. (Bindable t1, Bindable t2, Bindable p1, Bindable p2, Bindable i1, Bindable i2, Bindable m1, Bindable m2) => (t1 -> t2) -> (p1 -> p2) -> (m1 -> m2) ->  (i1 -> i2) -> Expression t1 p1 m1 i1 -> Expression t2 p2 m2 i2
mapExpr ft fp fm fi e = runIdentity $ traverseExpr ftm fpm fmm fim e
  where ftm = fmap pure ft
        fpm = fmap pure fp
        fmm = fmap pure fm
        fim = fmap pure fi


normalizeExprMetadata :: forall t p m i . (Bindable t, Bindable p, Bindable i, Bindable m) => Expression t p m i -> Expression () () m () 
normalizeExprMetadata = mapExpr (const ()) (const ()) id (const ())
