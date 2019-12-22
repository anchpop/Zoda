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

type Binder i = (Atom, NoBind i)



data Module phase i m = Module (ModuleHeader i) [(Declaration phase i m)] SourcePosition

deriving instance (ConstraintX Generic phase m i) => Generic (Module phase m i)
deriving instance (ConstraintX Nominal phase m i, ConstraintX Eq phase m i, Eq i, Nominal i, Eq m, Nominal m) => Eq (Module phase i m)
deriving instance (ConstraintX NominalShow phase m i, ConstraintX Show phase m i, Show i, NominalShow i, Show m, NominalShow m) => Show (Module phase i m)




data ModuleHeader i = ModuleHeader i (Tinydoc i) SourcePosition deriving (Show, Eq)



data Declaration phase i m = ValueDefinition i (ExpressionX phase i m) SourcePosition 
                           | ValueDefinitionAnnotated i (ExpressionX phase i m) SourcePosition (ExpressionX phase i m) SourcePosition
                           | TypeDefinition i (ExpressionX phase i m) SourcePosition [(i, (ExpressionX phase i m), SourcePosition)] SourcePosition
                         
deriving instance (ConstraintX Generic phase m i) => Generic (Declaration phase i m)
deriving instance (ConstraintX Nominal phase m i, ConstraintX Eq phase m i, Eq i, Nominal i, Eq m, Nominal m) => Eq (Declaration phase i m)
deriving instance (ConstraintX NominalShow phase m i, ConstraintX Show phase m i, Show i, NominalShow i, Show m, NominalShow m) => Show (Declaration phase i m)
deriving instance (ConstraintX Nominal phase m i, Nominal i, Nominal m) => Nominal (Declaration phase i m)
deriving instance (ConstraintX NominalShow phase m i, NominalShow i, NominalShow m) => NominalShow (Declaration phase i m)
deriving instance (ConstraintX NominalSupport phase m i, NominalSupport i, NominalSupport m) => NominalSupport (Declaration phase i m)




data ExpressionX phase i m = ParenthesizedExpressionX       (XParenthesizedExpression phase i m)       (ExpressionX phase i m) 
                           | FirstExpressionX               (XFirstExpression phase i m)               (ExpressionX phase i m) 
                           | SecondExpressionX              (XSecondExpression phase i m)              (ExpressionX phase i m) 
                           | PairExpressionX                (XPairExpression phase i m)                (ExpressionX phase i m)                          (ExpressionX phase i m) 
                           | TSigmaBindingX                 (XTSigmaBinding phase i m)                 (ExpressionX phase i m) (Bind (Maybe (Binder i)) (ExpressionX phase i m)) 
                           | UniverseExpressionX            (XUniverseExpression phase i m)            Integer 
                           | NumberLiteralX                 (XNumberLiteral phase i m)                 Rational 
                           | AddExpressionX                 (XAddExpression phase i m)                 (ExpressionX phase i m)                          (ExpressionX phase i m) 
                           | ReferenceVariableX             (XReferenceVariable phase i m)             i m
                           | LambdaVariableX                (XLambdaVariable phase i m)                (Atom, i) 
                           | FunctionLiteralExpressionX     (XFunctionLiteralExpression phase i m)     (FunctionLiteralX phase i m) 
                           | FunctionApplicationExpressionX (XFunctionApplicationExpression phase i m) (ExpressionX phase i m)                (NonEmpty (ExpressionX phase i m)) 
                           | TArrowBindingX                 (XTArrowBinding phase i m)                 (TelescopeX phase i m)
                           | AnnotationX                    (XAnnotation phase i m)                    (ExpressionX phase i m)                          (ExpressionX phase i m) 
                           | NatTypeExpressionX             (XNatTypeExpression phase i m)
                           | OtherX                         (XOther phase i m) 

deriving instance (ConstraintX Generic phase m i) => Generic (ExpressionX phase i m)
deriving instance (ConstraintX Nominal phase m i, ConstraintX Eq phase m i, Eq i, Nominal i, Eq m, Nominal m) => Eq (ExpressionX phase i m)
deriving instance (ConstraintX NominalShow phase m i, ConstraintX Show phase m i, Show i, NominalShow i, Show m, NominalShow m) => Show (ExpressionX phase i m)
deriving instance (ConstraintX Nominal phase m i, Nominal i, Nominal m) => Nominal (ExpressionX phase i m)
deriving instance (ConstraintX NominalShow phase m i, NominalShow i, NominalShow m) => NominalShow (ExpressionX phase i m)
deriving instance (ConstraintX NominalSupport phase m i, NominalSupport i, NominalSupport m) => NominalSupport (ExpressionX phase i m)



data TelescopeX phase i m = Scope (ExpressionX phase i m) (Bind (Maybe (Binder i)) (TelescopeX phase i m)) 
                          | Pi    (ExpressionX phase i m) (Bind (Maybe (Binder i)) (ExpressionX phase i m)) 


deriving instance (ConstraintX Generic phase m i) => Generic (TelescopeX phase i m)
deriving instance (ConstraintX Nominal phase m i, ConstraintX Eq phase m i, Eq i, Nominal i, Eq m, Nominal m) => Eq (TelescopeX phase i m)
deriving instance (ConstraintX NominalShow phase m i, ConstraintX Show phase m i, Show i, NominalShow i, Show m, NominalShow m) => Show (TelescopeX phase i m)
deriving instance (ConstraintX Nominal phase m i, Nominal i, Nominal m) => Nominal (TelescopeX phase i m)
deriving instance (ConstraintX NominalShow phase m i, NominalShow i, NominalShow m) => NominalShow (TelescopeX phase i m)
deriving instance (ConstraintX NominalSupport phase m i, NominalSupport i, NominalSupport m) => NominalSupport (TelescopeX phase i m)





data FunctionLiteralX phase i m = Arg     (ExpressionX phase i m) (Bind (Binder i) (FunctionLiteralX phase i m)) 
                                | LastArg (ExpressionX phase i m) (Bind (Binder i) (ExpressionX phase i m)) 

deriving instance (ConstraintX Generic phase m i) => Generic (FunctionLiteralX phase i m)
deriving instance (ConstraintX Nominal phase m i, ConstraintX Eq phase m i, Eq i, Nominal i, Eq m, Nominal m) => Eq (FunctionLiteralX phase i m)
deriving instance (ConstraintX NominalShow phase m i, ConstraintX Show phase m i, Show i, NominalShow i, Show m, NominalShow m) => Show (FunctionLiteralX phase i m)
deriving instance (ConstraintX Nominal phase m i, Nominal i, Nominal m) => Nominal (FunctionLiteralX phase i m)
deriving instance (ConstraintX NominalShow phase m i, NominalShow i, NominalShow m) => NominalShow (FunctionLiteralX phase i m)
deriving instance (ConstraintX NominalSupport phase m i, NominalSupport i, NominalSupport m) => NominalSupport (FunctionLiteralX phase i m)





data DelcarationInfo phase i m i' = Value (ExpressionX phase i m) 
                                  | ValueAndAnnotation (ExpressionX phase i m) (ExpressionX phase i m) 
                                  | TypeConstructor i' (ExpressionX phase i m) 
                                  | DataConstructor Int (ExpressionX phase i m) 

deriving instance (ConstraintX Generic phase m i) => Generic (DelcarationInfo phase i m i')
deriving instance (ConstraintX Nominal phase m i, ConstraintX Eq phase m i, Eq i, Nominal i, Eq m, Nominal m, Eq i', Nominal i') => Eq (DelcarationInfo phase i m i')
deriving instance (ConstraintX NominalShow phase m i, ConstraintX Show phase m i, Show i, NominalShow i, Show m, NominalShow m, Show i', NominalShow i') => Show (DelcarationInfo phase i m i')
deriving instance (ConstraintX Nominal phase m i, Nominal i, Nominal m, Nominal i') => Nominal (DelcarationInfo phase i m i')
deriving instance (ConstraintX NominalShow phase m i, NominalShow i, NominalShow m, NominalShow i') => NominalShow (DelcarationInfo phase i m i')
deriving instance (ConstraintX NominalSupport phase m i, NominalSupport i, NominalSupport m, NominalSupport i') => NominalSupport (DelcarationInfo phase i m i')




pattern ParenthesizedExpression e = ParenthesizedExpressionX () e
pattern FirstExpression e = FirstExpressionX () e
pattern SecondExpression e = SecondExpressionX () e
pattern PairExpression e1 e2 = PairExpressionX () e1 e2
pattern TSigmaBinding e1 e2 = TSigmaBindingX () e1 e2
pattern UniverseExpression e = UniverseExpressionX () e
pattern NumberLiteral e = NumberLiteralX () e
pattern AddExpression e1 e2 = AddExpressionX () e1 e2
pattern ReferenceVariable i m = ReferenceVariableX () i m
pattern LambdaVariable e = LambdaVariableX () e
pattern FunctionLiteralExpression e = FunctionLiteralExpressionX () e
pattern FunctionApplicationExpression e args = FunctionApplicationExpressionX () e args
pattern TArrowBinding e = TArrowBindingX () e
pattern Annotation e1 e2 = AnnotationX () e1 e2
pattern NatTypeExpression = NatTypeExpressionX ()
pattern Other = OtherX ()



-- Tags for different phases 
type family XParenthesizedExpression phase i m
type family XFirstExpression phase i m
type family XSecondExpression phase i m
type family XPairExpression phase i m
type family XTSigmaBinding phase i m
type family XUniverseExpression phase i m
type family XNumberLiteral phase i m
type family XAddExpression phase i m
type family XReferenceVariable phase i m
type family XLambdaVariable phase i m
type family XFunctionLiteralExpression phase i m
type family XFunctionApplicationExpression phase i m
type family XTArrowBinding phase i m
type family XAnnotation phase i m
type family XNatTypeExpression phase i m 
type family XOther phase i m

-- The parsed tag represents the AST directly after parsing
data Parsed 
type instance XParenthesizedExpression Parsed i m = ()
type instance XFirstExpression Parsed m i = ()
type instance XSecondExpression Parsed i m = ()
type instance XPairExpression Parsed i m = ()
type instance XUniverseExpression Parsed i m = ()
type instance XNumberLiteral Parsed i m = ()
type instance XAddExpression Parsed i m = ()
type instance XReferenceVariable Parsed i m = ()
type instance XLambdaVariable Parsed i m = ()
type instance XFunctionLiteralExpression Parsed i m = ()
type instance XFunctionApplicationExpression Parsed i m = ()
type instance XTArrowBinding Parsed i m = ()
type instance XAnnotation Parsed i m = ()
type instance XNatTypeExpression Parsed i m = ()
type instance XOther Parsed i m = Void


type ConstraintX constraint phase i m = (
    constraint (XParenthesizedExpression phase i m), 
    constraint (XFirstExpression phase i m), 
    constraint (XSecondExpression phase i m), 
    constraint (XPairExpression phase i m), 
    constraint (XTSigmaBinding phase i m), 
    constraint (XUniverseExpression phase i m), 
    constraint (XNumberLiteral phase i m), 
    constraint (XAddExpression phase i m), 
    constraint (XReferenceVariable phase i m), 
    constraint (XLambdaVariable phase i m), 
    constraint (XFunctionLiteralExpression phase i m), 
    constraint (XFunctionApplicationExpression phase i m), 
    constraint (XTArrowBinding phase i m), 
    constraint (XAnnotation phase i m), 
    constraint (XNatTypeExpression phase i m), 
    constraint (XOther phase i m), 
    
    Generic (XParenthesizedExpression phase i m), 
    Generic (XFirstExpression phase i m), 
    Generic (XSecondExpression phase i m), 
    Generic (XPairExpression phase i m), 
    Generic (XTSigmaBinding phase i m), 
    Generic (XUniverseExpression phase i m), 
    Generic (XNumberLiteral phase i m), 
    Generic (XAddExpression phase i m), 
    Generic (XReferenceVariable phase i m), 
    Generic (XLambdaVariable phase i m), 
    Generic (XFunctionLiteralExpression phase i m), 
    Generic (XFunctionApplicationExpression phase i m), 
    Generic (XTArrowBinding phase i m), 
    Generic (XAnnotation phase i m), 
    Generic (XNatTypeExpression phase i m), 
    Generic (XOther phase i m)
  )




data Tinydoc i = Tinydoc i deriving (Show, Read, Eq, Ord, Generic, Typeable)

data Untyped = Untyped deriving (Show, Read, Eq, Ord, NominalSupport, NominalShow, Generic, Nominal, Bindable)


type JustifiedModule           ph phase m i i' = Map.Map ph m (DelcarationInfo phase i (Map.Key ph m) i')
type JustifiedExpression       ph phase m i    = ExpressionX phase i (Map.Key ph m)
type JustifiedTelescopeX       ph phase m i    = TelescopeX phase i (Map.Key ph m)
type JustifiedFunctionLiteralX ph phase m i    = FunctionLiteralX phase i (Map.Key ph m)





data SourcePosition = SourcePosition {_filePath :: String, _sourceLineStart :: Int, _sourceColumnStart  :: Int, _sourceLineEnd :: Int, _sourceColumnEnd  :: Int} 
                    | Base 
                    deriving (Read, Eq, NominalSupport, NominalShow, Generic, Nominal, Bindable)
instance Show SourcePosition where
  --show _ = ""
  show (SourcePosition f l1 c1 l2 c2) = "(SourcePosition \"" <> f <> "\" " <> (show l1) <> " " <> (show c1) <> " " <> (show l2) <> " " <> (show c2) <> ")"
  show (Base) = "Base" 




getOutputOfScope :: (ConstraintX Nominal phase m i, Nominal t, Nominal p, Nominal m, Nominal i) => TelescopeX phase i -> ExpressionX phase i 
getOutputOfScope (Scope _ (_ :. scope) ) = getOutputOfScope scope
getOutputOfScope (Pi    _ (_ :. e) ) = e

getBodyOfFlit :: (ConstraintX Nominal phase m i, Nominal t, Nominal p, Nominal m, Nominal i) => FunctionLiteralX phase i -> ExpressionX phase i 
getBodyOfFlit (Arg     _ (_ :. scope)) = getBodyOfFlit scope
getBodyOfFlit (LastArg _ (_ :. e)) = e




substExpr :: (ConstraintX Nominal phase m i, Nominal t, Nominal p, Nominal i, Nominal m) => Atom -> ExpressionX phase i -> ExpressionX phase i -> ExpressionX phae ii
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

substTelescope :: (ConstraintX Nominal phase m i, Nominal t, Nominal p, Nominal i, Nominal m) => Atom -> ExpressionX phase i -> TelescopeX phase i -> TelescopeX phase i
substTelescope lookingFor substWith = subst 
  where substE = substExpr lookingFor substWith
        subst (Scope e1 (a :. scope)) = Scope (substE e1) (a :. subst scope)
        subst (Pi    e1 (a :. e2   )) = Pi (substE e1) (a :. substE e2)
substFlit :: (ConstraintX Nominal phase m i, Nominal t, Nominal p, Nominal i, Nominal m) => Atom -> ExpressionX phase i -> FunctionLiteralX phase i -> FunctionLiteralX phase i
substFlit lookingFor substWith = subst
  where substE = substExpr lookingFor substWith
        subst (Arg     e1 (a :. scope)) = Arg     (substE e1) (a :. subst scope)
        subst (LastArg e1 (a :. e2   )) = LastArg (substE e1) (a :. substE e2)

liftA4 :: Applicative f => (a1 -> b1 -> c -> a2 -> b2) -> f a1 -> f b1 -> f c -> f a2 -> f b2
liftA4 f a b c d = liftA3 f a b c <*> d

traverseExpr :: forall t1 p1 m1 i1 t2 p2 m2 i2 a phase. (ConstraintX Bindable phase t1 p1 m1 i1, ConstraintX Bindable phase t2 p2 m2 i2, Bindable t1, Bindable t2, Bindable p1, Bindable p2, Bindable i1, Bindable i2, Bindable m1, Bindable m2, Applicative a) => (t1 -> a t2) -> (p1 -> a p2) -> (m1 -> a m2) ->  (i1 -> a i2) -> ExpressionX phase t1 p1 m1 i1 -> a (ExpressionX phase t2 p2 m2 i2)
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

traverseTelescope :: forall t1 p1 m1 i1 t2 p2 m2 i2 a phase. (ConstraintX Bindable phase t1 p1 m1 i1, ConstraintX Bindable phase t2 p2 m2 i2, Bindable t1, Bindable t2, Bindable p1, Bindable p2, Bindable i1, Bindable i2, Bindable m1, Bindable m2, Applicative a) => (t1 -> a t2) -> (p1 -> a p2) -> (m1 -> a m2) ->  (i1 -> a i2) -> TelescopeX phase t1 p1 m1 i1 -> a (TelescopeX phase t2 p2 m2 i2)
traverseTelescope ft fp fm fi = te
  where te (Scope e ((Just (a, NoBind i, NoBind p)) :. scope))   = liftA2 Scope (me e) (liftA2 abst (liftA3 filler (pure a) (fi i) (fp p)) (te scope))
        te (Scope e ((Nothing                       :. scope)))  = liftA2 Scope (me e) (liftA2 abst (pure Nothing)                         (te scope))
        te (Pi    e ((Just (a, NoBind i, NoBind p)) :. eBound))  = liftA2 Pi    (me e) (liftA2 abst (liftA3 filler (pure a) (fi i) (fp p)) (me eBound))
        te (Pi    e ((Nothing                       :. eBound))) = liftA2 Pi    (me e) (liftA2 abst (pure Nothing)                         (me eBound))
        filler a i p = Just (a, NoBind i, NoBind p)
        me = traverseExpr ft fp fm fi


traverseFunctionLiteral :: forall i1 m1 i2 m2 a phase. (ConstraintX Bindable phase i1 m1, ConstraintX Bindable phase i2 m2, Bindable i1, Bindable i2, Bindable m1, Bindable m2, Applicative a) => (i1 -> a i2) -> (m1 -> a m2) -> FunctionLiteralX phase i1 m1 -> a (FunctionLiteralX phase i2 m2)
traverseFunctionLiteral fi fm = fe
  where fe (Arg     e (((a, NoBind i)) :. scope))   = liftA2 Arg     (me e) (liftA2 abst (liftA (binder a) (fi i)) (fe scope))
        fe (LastArg e (((a, NoBind i)) :. eBound))  = liftA2 LastArg (me e) (liftA2 abst (liftA (binder a) (fi i)) (me eBound))
        binder a i = (a, NoBind i)
        me = traverseExpr fi fm

{-
traverseExpr2 :: (ConstraintX Bindable phase t p m1 i, ConstraintX Bindable phase t p m2 i, Bindable t, Bindable p, Bindable i, Bindable m1, Bindable m2, Monad mo) => (m1 -> mo m2) -> ExpressionX phase t p m1 i -> mo (ExpressionX phase t p m2 i)
traverseExpr2 fm = traverseExpr (fmap pure id) (fmap pure id) (fm) (fmap pure id)

forExpr2 :: (ConstraintX Bindable phase t p m1 i, ConstraintX Bindable phase t p m2 i, Bindable t, Bindable p, Bindable i, Monad mo, Bindable m1, Bindable m2) => ExpressionX phase t p m1 i -> (m1 -> mo m2) -> mo (ExpressionX phase t p m2 i)
forExpr2 x y = traverseExpr2 y x


mapExpr :: forall t1 p1 m1 i1 t2 p2 m2 i2 phase. (ConstraintX Bindable phase t1 p1 m1 i1, ConstraintX Bindable phase t2 p2 m2 i2, Bindable t1, Bindable t2, Bindable p1, Bindable p2, Bindable i1, Bindable i2, Bindable m1, Bindable m2) => (t1 -> t2) -> (p1 -> p2) -> (m1 -> m2) ->  (i1 -> i2) -> ExpressionX phase t1 p1 m1 i1 -> ExpressionX phase t2 p2 m2 i2
mapExpr ft fp fm fi e = runIdentity $ traverseExpr ftm fpm fmm fim e
  where ftm = fmap pure ft
        fpm = fmap pure fp
        fmm = fmap pure fm
        fim = fmap pure fi


normalizeExprMetadata :: forall t p m i phase. (ConstraintX Bindable phase m i, Bindable t, Bindable p, Bindable i, Bindable m) => ExpressionX phase i -> ExpressionX phase () () m () 
normalizeExprMetadata = mapExpr (const ()) (const ()) id (const ())
normalizeDelcarationInfoMetadata :: forall t p m i i' phase. (ConstraintX Bindable phase m i, Bindable t, Bindable p, Bindable i, Bindable m) => (DelcarationInfo phase i i') -> (DelcarationInfo phase () () m () i')
normalizeDelcarationInfoMetadata (Value e) = Value $ normalizeExprMetadata e
normalizeDelcarationInfoMetadata (ValueAndAnnotation e t) = ValueAndAnnotation (normalizeExprMetadata e) (normalizeExprMetadata t)
normalizeDelcarationInfoMetadata (TypeConstructor i e) = TypeConstructor i $ normalizeExprMetadata e
normalizeDelcarationInfoMetadata (DataConstructor i e) = DataConstructor i $ normalizeExprMetadata e

getValueFromDelcarationInfo (Value v) = v 
getValueFromDelcarationInfo (ValueAndAnnotation v _) = v 
getValueFromDelcarationInfo (TypeConstructor _ _) = error "Expected value, got constructor!" 
getValueFromDelcarationInfo (DataConstructor _ _) = error "Expected value, got constructor!" 
-}