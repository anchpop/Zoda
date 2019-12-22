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

type Applier constraint phase = (constraint (XParenthesizedExpression phase), constraint (XFirstExpression phase), constraint (XSecondExpression phase), constraint (XPairExpression phase), constraint (XTSigmaBinding phase), constraint (XUniverseExpression phase), constraint (XNumberLiteral phase), constraint (XAddExpression phase), constraint (XReferenceVariable phase), constraint (XLambdaVariable phase), constraint (XFunctionLiteralExpression phase), constraint (XFunctionApplicationExpression phase), constraint (XTArrowBinding phase), constraint (XAnnotation phase), constraint (XNatTypeExpression phase), constraint (XOther phase))
type ConstraintX constraint phase = (constraint (XParenthesizedExpression phase), constraint (XFirstExpression phase), constraint (XSecondExpression phase), constraint (XPairExpression phase), constraint (XTSigmaBinding phase), constraint (XUniverseExpression phase), constraint (XNumberLiteral phase), constraint (XAddExpression phase), constraint (XReferenceVariable phase), constraint (XLambdaVariable phase), constraint (XFunctionLiteralExpression phase), constraint (XFunctionApplicationExpression phase), constraint (XTArrowBinding phase), constraint (XAnnotation phase), constraint (XNatTypeExpression phase), constraint (XOther phase), Generic (XParenthesizedExpression phase), Generic (XFirstExpression phase), Generic (XSecondExpression phase), Generic (XPairExpression phase), Generic (XTSigmaBinding phase), Generic (XUniverseExpression phase), Generic (XNumberLiteral phase), Generic (XAddExpression phase), Generic (XReferenceVariable phase), Generic (XLambdaVariable phase), Generic (XFunctionLiteralExpression phase), Generic (XFunctionApplicationExpression phase), Generic (XTArrowBinding phase), Generic (XAnnotation phase), Generic (XNatTypeExpression phase), Generic (XOther phase))

type Binder i p = (Atom, NoBind i, NoBind p)



data Module phase t p m i = Module (ModuleHeader t p m i) [(Declaration phase t p m i)] p 

deriving instance (ConstraintX Generic phase) => Generic (Module phase t p m i)
deriving instance (ConstraintX Nominal phase, ConstraintX Eq phase, Eq t, Eq p, Eq i, Eq m, Nominal i, Nominal p, Nominal t, Nominal m) => Eq (Module phase t p m i)
deriving instance (ConstraintX NominalShow phase, ConstraintX Show phase, Show t, Show p, Show i, Show m, NominalShow i, NominalShow p, NominalShow t, NominalShow m) => Show (Module phase t p m i)




data ModuleHeader t p m i = ModuleHeader i (Tinydoc t p m i) p deriving (Show, Eq)



data Declaration phase t p m i = ValueDefinition i (ExpressionX phase t p m i) p 
                               | ValueDefinitionAnnotated i (ExpressionX phase t p m i) p (ExpressionX phase t p m i) p p
                               | TypeDefinition i (ExpressionX phase t p m i) p [(i, (ExpressionX phase t p m i), p)] p 
                         
deriving instance (ConstraintX Generic phase) => Generic (Declaration phase t p m i)
deriving instance (ConstraintX Nominal phase, ConstraintX Eq phase, Eq t, Eq p, Eq i, Eq m, Nominal i, Nominal p, Nominal t, Nominal m) => Eq (Declaration phase t p m i)
deriving instance (ConstraintX NominalShow phase, ConstraintX Show phase, Show t, Show p, Show i, Show m, NominalShow i, NominalShow p, NominalShow t, NominalShow m) => Show (Declaration phase t p m i)
deriving instance (ConstraintX Nominal phase, Nominal t, Nominal p, Nominal i, Nominal m) => Nominal (Declaration phase t p m i)
deriving instance (ConstraintX NominalShow phase, NominalShow t, NominalShow p, NominalShow i, NominalShow m) => NominalShow (Declaration phase t p m i)
deriving instance (ConstraintX NominalSupport phase, NominalSupport t, NominalSupport p, NominalSupport i, NominalSupport m) => NominalSupport (Declaration phase t p m i)




data ExpressionX phase t p m i = ParenthesizedExpressionX (XParenthesizedExpression phase)       (ExpressionX phase t p m i)                                              t p 
                               | FirstExpressionX (XFirstExpression phase)               (ExpressionX phase t p m i)                                                     t p 
                               | SecondExpressionX (XSecondExpression phase)              (ExpressionX phase t p m i)                                                     t p 
                               | PairExpressionX (XPairExpression phase)                (ExpressionX phase t p m i)                         (ExpressionX phase t p m i)  t p 
                               | TSigmaBindingX (XTSigmaBinding phase)                 (ExpressionX phase t p m i) (Bind (Maybe (Binder i p)) (ExpressionX phase t p m i)) t p 
                               | UniverseExpressionX (XUniverseExpression phase) Integer                                                                                    t p 
                               | NumberLiteralX (XNumberLiteral phase) Rational                                                                                        t p 
                               | AddExpressionX (XAddExpression phase)                 (ExpressionX phase t p m i)                          (ExpressionX phase t p m i)  t p 
                               | ReferenceVariableX (XReferenceVariable phase) i m                                                                                         t p 
                               | LambdaVariableX (XLambdaVariable phase) (Atom, i)                                                                                      t p 
                               | FunctionLiteralExpressionX (XFunctionLiteralExpression phase)     (FunctionLiteralX phase t p m i)                                        t p 
                               | FunctionApplicationExpressionX (XFunctionApplicationExpression phase) (ExpressionX phase t p m i)      (NonEmpty (ExpressionX phase t p m i)) t p 
                               | TArrowBindingX (XTArrowBinding phase)                (TelescopeX phase  t p m i)                                                     t p
                               | AnnotationX (XAnnotation phase)                   (ExpressionX phase t p m i)                          (ExpressionX phase t p m i)  t p 
                               | NatTypeExpressionX (XNatTypeExpression phase)                                                                  t p
                               | OtherX (XOther phase) 

deriving instance (ConstraintX Generic phase) => Generic (ExpressionX phase t p m i)
deriving instance (ConstraintX Nominal phase, ConstraintX Eq phase, Eq t, Eq p, Eq i, Eq m, Nominal i, Nominal p, Nominal t, Nominal m) => Eq (ExpressionX phase t p m i)
deriving instance (ConstraintX NominalShow phase, ConstraintX Show phase, Show t, Show p, Show i, Show m, NominalShow i, NominalShow p, NominalShow t, NominalShow m) => Show (ExpressionX phase t p m i)
deriving instance (ConstraintX Nominal phase, Nominal t, Nominal p, Nominal i, Nominal m) => Nominal (ExpressionX phase t p m i)
deriving instance (ConstraintX NominalShow phase, NominalShow t, NominalShow p, NominalShow i, NominalShow m) => NominalShow (ExpressionX phase t p m i)
deriving instance (ConstraintX NominalSupport phase, NominalSupport t, NominalSupport p, NominalSupport i, NominalSupport m) => NominalSupport (ExpressionX phase t p m i)


pattern ParenthesizedExpression e t p = ParenthesizedExpressionX () e t p
pattern FirstExpression e t p = FirstExpressionX () e t p
pattern SecondExpression e t p = SecondExpressionX () e t p
pattern PairExpression e1 e2 t p = PairExpressionX () e1 e2 t p
pattern TSigmaBinding e1 e2 t p = TSigmaBindingX () e1 e2 t p
pattern UniverseExpression e t p = UniverseExpressionX () e t p
pattern NumberLiteral e t p = NumberLiteralX () e t p
pattern AddExpression e1 e2 t p = AddExpressionX () e1 e2 t p
pattern ReferenceVariable i m t p = ReferenceVariableX () i m t p
pattern LambdaVariable e t p = LambdaVariableX () e t p
pattern FunctionLiteralExpression e t p = FunctionLiteralExpressionX () e t p
pattern FunctionApplicationExpression e args t p = FunctionApplicationExpressionX () e args t p
pattern TArrowBinding e t p = TArrowBindingX () e t p
pattern Annotation e1 e2 t p = AnnotationX () e1 e2 t p
pattern NatTypeExpression t p = NatTypeExpressionX () t p
pattern Other = OtherX ()


data TelescopeX phase t p m i = Scope (ExpressionX phase t p m i) (Bind (Maybe (Binder i p)) (TelescopeX phase t p m i)) 
                              | Pi    (ExpressionX phase t p m i) (Bind (Maybe (Binder i p)) (ExpressionX phase t p m i)) 

deriving instance (ConstraintX Generic phase) => Generic (TelescopeX phase t p m i)
deriving instance (ConstraintX Nominal phase, ConstraintX Eq phase, Eq t, Eq p, Eq i, Eq m, Nominal i, Nominal p, Nominal t, Nominal m) => Eq (TelescopeX phase t p m i)
deriving instance (ConstraintX NominalShow phase, ConstraintX Show phase, Show t, Show p, Show i, Show m, NominalShow i, NominalShow p, NominalShow t, NominalShow m) => Show (TelescopeX phase t p m i)
deriving instance (ConstraintX Nominal phase, Nominal t, Nominal p, Nominal i, Nominal m) => Nominal (TelescopeX phase t p m i)
deriving instance (ConstraintX NominalShow phase, NominalShow t, NominalShow p, NominalShow i, NominalShow m) => NominalShow (TelescopeX phase t p m i)
deriving instance (ConstraintX NominalSupport phase, NominalSupport t, NominalSupport p, NominalSupport i, NominalSupport m) => NominalSupport (TelescopeX phase t p m i)




data FunctionLiteralX phase t p m i = Arg     (ExpressionX phase t p m i) (Bind (Binder i p) (FunctionLiteralX phase t p m i)) 
                                    | LastArg (ExpressionX phase t p m i) (Bind (Binder i p) (ExpressionX phase t p m i)) 

deriving instance (ConstraintX Generic phase) => Generic (FunctionLiteralX phase t p m i)
deriving instance (ConstraintX Nominal phase, ConstraintX Eq phase, Eq t, Eq p, Eq i, Eq m, Nominal i, Nominal p, Nominal t, Nominal m) => Eq (FunctionLiteralX phase t p m i)
deriving instance (ConstraintX NominalShow phase, ConstraintX Show phase, Show t, Show p, Show i, Show m, NominalShow i, NominalShow p, NominalShow t, NominalShow m) => Show (FunctionLiteralX phase t p m i)
deriving instance (ConstraintX Nominal phase, Nominal t, Nominal p, Nominal i, Nominal m) => Nominal (FunctionLiteralX phase t p m i)
deriving instance (ConstraintX NominalShow phase, NominalShow t, NominalShow p, NominalShow i, NominalShow m) => NominalShow (FunctionLiteralX phase t p m i)
deriving instance (ConstraintX NominalSupport phase, NominalSupport t, NominalSupport p, NominalSupport i, NominalSupport m) => NominalSupport (FunctionLiteralX phase t p m i)

data DelcarationInfo phase t p m i i' = Value (ExpressionX phase t p m i) | ValueAndAnnotation (ExpressionX phase t p m i) (ExpressionX phase t p m i) | TypeConstructor i' (ExpressionX phase t p m i) | DataConstructor Int (ExpressionX phase t p m i) 

deriving instance (ConstraintX Generic phase) => Generic (DelcarationInfo phase t p m i i')
deriving instance (ConstraintX Nominal phase, ConstraintX Eq phase, Eq t, Eq p, Eq i, Eq i', Eq m, Nominal i, Nominal i', Nominal p, Nominal t, Nominal m) => Eq (DelcarationInfo phase t p m i i')
deriving instance (ConstraintX NominalShow phase, ConstraintX Show phase, Show t, Show p, Show i, Show i', Show m, NominalShow i, NominalShow i', NominalShow p, NominalShow t, NominalShow m) => Show (DelcarationInfo phase t p m i i')
deriving instance (ConstraintX Nominal phase, Nominal t, Nominal p, Nominal i, Nominal i', Nominal m) => Nominal (DelcarationInfo phase t p m i i')
deriving instance (ConstraintX NominalShow phase, NominalShow t, NominalShow p, NominalShow i, NominalShow i', NominalShow m) => NominalShow (DelcarationInfo phase t p m i i')
deriving instance (ConstraintX NominalSupport phase, NominalSupport t, NominalSupport p, NominalSupport i, NominalSupport i', NominalSupport m) => NominalSupport (DelcarationInfo phase t p m i i')




-- Tags for different phases 
type family XParenthesizedExpression phase 
type family XFirstExpression phase
type family XSecondExpression phase
type family XPairExpression phase
type family XTSigmaBinding phase
type family XUniverseExpression phase
type family XNumberLiteral phase
type family XAddExpression phase
type family XReferenceVariable phase
type family XLambdaVariable phase
type family XFunctionLiteralExpression phase
type family XFunctionApplicationExpression phase
type family XTArrowBinding phase
type family XAnnotation phase
type family XNatTypeExpression phase
type family XOther phase

data Parsed deriving (Show, Eq, Typeable, NominalSupport, NominalShow, Generic, Nominal)
type instance XParenthesizedExpression Parsed = ()
type instance XFirstExpression Parsed = ()
type instance XSecondExpression Parsed = ()
type instance XPairExpression Parsed = ()
type instance XUniverseExpression Parsed = ()
type instance XNumberLiteral Parsed = ()
type instance XAddExpression Parsed = ()
type instance XReferenceVariable Parsed = ()
type instance XLambdaVariable Parsed = ()
type instance XFunctionLiteralExpression Parsed = ()
type instance XFunctionApplicationExpression Parsed = ()
type instance XTArrowBinding Parsed = ()
type instance XAnnotation Parsed = ()
type instance XNatTypeExpression Parsed = ()
type instance XOther Parsed = Void





data Tinydoc t p m i = Tinydoc Text p deriving (Show, Read, Eq, Ord, Generic, Typeable)

data Untyped = Untyped deriving (Show, Read, Eq, Ord, NominalSupport, NominalShow, Generic, Nominal, Bindable)

instance Representational (Tinydoc t p) where rep Coercion = Coercion
instance Representational (ExpressionX phase t p) where rep Coercion = Coercion
instance Representational (Declaration t p) where rep Coercion = Coercion
instance Representational (ModuleHeader t p) where rep Coercion = Coercion
instance Representational (Module t p) where rep Coercion = Coercion

type JustifiedModule     phase t p ph m i i'    = Map.Map ph m (DelcarationInfo phase t p (Map.Key ph m) i i')
type JustifiedExpression phase t p ph m i       = ExpressionX phase t p (Map.Key ph m) i
type JustifiedTelescopeX phase t p ph m i       = TelescopeX phase t p (Map.Key ph m) i
type JustifiedFunctionLiteralX phase t p ph m i = FunctionLiteralX phase t p (Map.Key ph m) i


getOutputOfScope :: (ConstraintX Nominal phase, Nominal t, Nominal p, Nominal m, Nominal i) => TelescopeX phase t p m i -> ExpressionX phase t p m i 
getOutputOfScope (Scope _ (_ :. scope) ) = getOutputOfScope scope
getOutputOfScope (Pi    _ (_ :. e) ) = e

getBodyOfFlit :: (ConstraintX Nominal phase, Nominal t, Nominal p, Nominal m, Nominal i) => FunctionLiteralX phase t p m i -> ExpressionX phase t p m i 
getBodyOfFlit (Arg     _ (_ :. scope)) = getBodyOfFlit scope
getBodyOfFlit (LastArg _ (_ :. e)) = e




substExpr :: (ConstraintX Nominal phase, Nominal t, Nominal p, Nominal i, Nominal m) => Atom -> ExpressionX phase t p m i -> ExpressionX phase t p m i -> ExpressionX phase t p m i
substExpr lookingFor substWith = subst 
  where subst (ParenthesizedExpressionX inf e t p) = ParenthesizedExpressionX inf (subst e) t p
        subst (FirstExpressionX inf e t p) = FirstExpressionX inf (subst e) t p
        subst (SecondExpressionX inf e t p) = SecondExpressionX inf (subst e) t p
        subst (PairExpressionX inf e1 e2 t p) = PairExpressionX inf (subst e1) (subst e2) t p
        subst (TSigmaBindingX inf e1 (a :. e2) t p) = TSigmaBindingX inf (subst e1) (a :. (subst e2)) t p
        subst (UniverseExpressionX inf i t p) = UniverseExpressionX inf i t p
        subst (NumberLiteralX inf i t p) = NumberLiteralX inf i t p
        subst (AddExpressionX inf e1 e2 t p) = AddExpressionX inf (subst e1) (subst e2) t p
        subst (ReferenceVariableX inf i m t p) = ReferenceVariableX inf i m t p
        subst (LambdaVariableX inf (a, i) t p) = if a == lookingFor then substWith else LambdaVariableX inf (a, i) t p
        subst (FunctionApplicationExpressionX inf func args t p) = FunctionApplicationExpressionX inf (subst func) (fmap subst args) t p
        subst (TArrowBindingX inf scope t p) = TArrowBindingX inf (substTelescope lookingFor substWith scope) t p
        subst (AnnotationX inf e1 e2 t p) = AnnotationX inf (subst e1) (subst e2) t p
        subst (NatTypeExpressionX inf t p) = NatTypeExpressionX inf t p
        subst (FunctionLiteralExpressionX inf flit t p) = FunctionLiteralExpressionX inf (substFlit lookingFor substWith flit) t p

substTelescope :: (ConstraintX Nominal phase, Nominal t, Nominal p, Nominal i, Nominal m) => Atom -> ExpressionX phase t p m i -> TelescopeX phase t p m i -> TelescopeX phase t p m i
substTelescope lookingFor substWith = subst 
  where substE = substExpr lookingFor substWith
        subst (Scope e1 (a :. scope)) = Scope (substE e1) (a :. subst scope)
        subst (Pi    e1 (a :. e2   )) = Pi (substE e1) (a :. substE e2)
substFlit :: (ConstraintX Nominal phase, Nominal t, Nominal p, Nominal i, Nominal m) => Atom -> ExpressionX phase t p m i -> FunctionLiteralX phase t p m i -> FunctionLiteralX phase t p m i
substFlit lookingFor substWith = subst
  where substE = substExpr lookingFor substWith
        subst (Arg     e1 (a :. scope)) = Arg     (substE e1) (a :. subst scope)
        subst (LastArg e1 (a :. e2   )) = LastArg (substE e1) (a :. substE e2)

liftA4 :: Applicative f => (a1 -> b1 -> c -> a2 -> b2) -> f a1 -> f b1 -> f c -> f a2 -> f b2
liftA4 f a b c d = liftA3 f a b c <*> d

traverseExpr :: forall t1 p1 m1 i1 t2 p2 m2 i2 a phase. (ConstraintX Bindable phase, Bindable t1, Bindable t2, Bindable p1, Bindable p2, Bindable i1, Bindable i2, Bindable m1, Bindable m2, Applicative a) => (t1 -> a t2) -> (p1 -> a p2) -> (m1 -> a m2) ->  (i1 -> a i2) -> ExpressionX phase t1 p1 m1 i1 -> a (ExpressionX phase t2 p2 m2 i2)
traverseExpr ft fp fm fi = me
  where me :: ExpressionX phase t1 p1 m1 i1 -> a (ExpressionX phase t2 p2 m2 i2)
        me (ParenthesizedExpressionX inf e t p) = liftA3 (ParenthesizedExpressionX inf) (me e) (ft t) (fp p)
        me (FirstExpressionX inf e t p)         = (liftA3 (FirstExpressionX inf)) (me e) (ft t) (fp p)
        me (SecondExpressionX inf e t p)        = (liftA3 (SecondExpressionX inf)) (me e) (ft t) (fp p)
        me (PairExpressionX inf e1 e2 t p)      = (liftA4 (PairExpressionX inf)) (me e1) (me e2) (ft t) (fp p)
        me (TSigmaBindingX inf e1 (Just (a, NoBind i, NoBind p1) :. e2) t p2) = liftA4 (TSigmaBindingX inf) (me e1) (liftA2 abst (liftA2 (\x y -> Just (a, NoBind x, NoBind y)) (fi i) (fp p1)) (me e2)) (ft t) (fp p2)
        me (TSigmaBindingX inf e1 (Nothing                       :. e2) t p2) = liftA4 (TSigmaBindingX inf) (me e1) (liftA2 abst (pure Nothing) (me e2)) (ft t) (fp p2)
        me (UniverseExpressionX inf i t p)      = (liftA2 (UniverseExpressionX inf i)) (ft t) (fp p)
        me (NumberLiteralX inf i t p)           = (liftA2 (NumberLiteralX inf i)) (ft t) (fp p)
        me (AddExpressionX inf e1 e2 t p)       = (liftA4 (AddExpressionX inf)) (me e1) (me e2) (ft t) (fp p)
        me (ReferenceVariableX inf i m t p)     = (liftA4 (ReferenceVariableX inf)) (fi i) (fm m) (ft t) (fp p)
        me (LambdaVariableX inf (a, i) t p)     = (liftA3 (LambdaVariableX inf)) ((\x -> (a, x)) <$> fi i) (ft t) (fp p)
        me (FunctionLiteralExpressionX inf flit t p) = (liftA3 (FunctionLiteralExpressionX inf)) (traverseFunctionLiteral ft fp fm fi flit) (ft t) (fp p)
        me (FunctionApplicationExpressionX inf func args t p) = liftA4 (FunctionApplicationExpressionX inf) (me func) (traverse me args) (ft t) (fp p)
        me (TArrowBindingX inf telescope t p) = (liftA3 (TArrowBindingX inf)) (traverseTelescope ft fp fm fi telescope) (ft t) (fp p)
        me (AnnotationX inf e1 e2 t p) = (liftA4 (AnnotationX inf)) (me e1) (me e2) (ft t) (fp p)
        me (NatTypeExpressionX inf t p) = (liftA2 (NatTypeExpressionX inf)) (ft t) (fp p)

traverseTelescope :: forall t1 p1 m1 i1 t2 p2 m2 i2 a phase. (ConstraintX Bindable phase, Bindable t1, Bindable t2, Bindable p1, Bindable p2, Bindable i1, Bindable i2, Bindable m1, Bindable m2, Applicative a) => (t1 -> a t2) -> (p1 -> a p2) -> (m1 -> a m2) ->  (i1 -> a i2) -> TelescopeX phase t1 p1 m1 i1 -> a (TelescopeX phase t2 p2 m2 i2)
traverseTelescope ft fp fm fi = te
  where te (Scope e ((Just (a, NoBind i, NoBind p)) :. scope))   = liftA2 Scope (me e) (liftA2 abst (liftA3 filler (pure a) (fi i) (fp p)) (te scope))
        te (Scope e ((Nothing                       :. scope)))  = liftA2 Scope (me e) (liftA2 abst (pure Nothing)                         (te scope))
        te (Pi    e ((Just (a, NoBind i, NoBind p)) :. eBound))  = liftA2 Pi    (me e) (liftA2 abst (liftA3 filler (pure a) (fi i) (fp p)) (me eBound))
        te (Pi    e ((Nothing                       :. eBound))) = liftA2 Pi    (me e) (liftA2 abst (pure Nothing)                         (me eBound))
        filler a i p = Just (a, NoBind i, NoBind p)
        me = traverseExpr ft fp fm fi


traverseFunctionLiteral :: forall t1 p1 m1 i1 t2 p2 m2 i2 a phase. (ConstraintX Bindable phase, Bindable t1, Bindable t2, Bindable p1, Bindable p2, Bindable i1, Bindable i2, Bindable m1, Bindable m2, Applicative a) => (t1 -> a t2) -> (p1 -> a p2) -> (m1 -> a m2) ->  (i1 -> a i2) -> FunctionLiteralX phase t1 p1 m1 i1 -> a (FunctionLiteralX phase t2 p2 m2 i2)
traverseFunctionLiteral ft fp fm fi = fe
  where fe (Arg     e (((a, NoBind i, NoBind p)) :. scope))   = liftA2 Arg     (me e) (liftA2 abst (liftA3 filler (pure a) (fi i) (fp p)) (fe scope))
        fe (LastArg e (((a, NoBind i, NoBind p)) :. eBound))  = liftA2 LastArg (me e) (liftA2 abst (liftA3 filler (pure a) (fi i) (fp p)) (me eBound))
        filler a i p = (a, NoBind i, NoBind p)
        me = traverseExpr ft fp fm fi

traverseExpr2 :: (ConstraintX Bindable phase, Bindable t, Bindable p, Bindable i, Bindable m1, Bindable m2, Monad mo) => (m1 -> mo m2) -> ExpressionX phase t p m1 i -> mo (ExpressionX phase t p m2 i)
traverseExpr2 fm = traverseExpr (fmap pure id) (fmap pure id) (fm) (fmap pure id)

forExpr2 :: (ConstraintX Bindable phase, Bindable t, Bindable p, Bindable i, Monad mo, Bindable m1, Bindable m2) => ExpressionX phase t p m1 i -> (m1 -> mo m2) -> mo (ExpressionX phase t p m2 i)
forExpr2 x y = traverseExpr2 y x


mapExpr :: forall t1 p1 m1 i1 t2 p2 m2 i2 phase. (ConstraintX Bindable phase, Bindable t1, Bindable t2, Bindable p1, Bindable p2, Bindable i1, Bindable i2, Bindable m1, Bindable m2) => (t1 -> t2) -> (p1 -> p2) -> (m1 -> m2) ->  (i1 -> i2) -> ExpressionX phase t1 p1 m1 i1 -> ExpressionX phase t2 p2 m2 i2
mapExpr ft fp fm fi e = runIdentity $ traverseExpr ftm fpm fmm fim e
  where ftm = fmap pure ft
        fpm = fmap pure fp
        fmm = fmap pure fm
        fim = fmap pure fi


normalizeExprMetadata :: forall t p m i phase. (ConstraintX Bindable phase, Bindable t, Bindable p, Bindable i, Bindable m) => ExpressionX phase t p m i -> ExpressionX phase () () m () 
normalizeExprMetadata = mapExpr (const ()) (const ()) id (const ())
normalizeDelcarationInfoMetadata :: forall t p m i i' phase. (ConstraintX Bindable phase, Bindable t, Bindable p, Bindable i, Bindable m) => (DelcarationInfo phase t p m i i') -> (DelcarationInfo phase () () m () i')
normalizeDelcarationInfoMetadata (Value e) = Value $ normalizeExprMetadata e
normalizeDelcarationInfoMetadata (ValueAndAnnotation e t) = ValueAndAnnotation (normalizeExprMetadata e) (normalizeExprMetadata t)
normalizeDelcarationInfoMetadata (TypeConstructor i e) = TypeConstructor i $ normalizeExprMetadata e
normalizeDelcarationInfoMetadata (DataConstructor i e) = DataConstructor i $ normalizeExprMetadata e

getValueFromDelcarationInfo (Value v) = v 
getValueFromDelcarationInfo (ValueAndAnnotation v _) = v 
getValueFromDelcarationInfo (TypeConstructor _ _) = error "Expected value, got constructor!" 
getValueFromDelcarationInfo (DataConstructor _ _) = error "Expected value, got constructor!" 