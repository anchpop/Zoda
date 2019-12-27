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



data Module phase i m = Module (ModuleHeader i) [(Declaration phase i m)]

deriving instance (ConstraintX Generic phase i m) => Generic (Module phase m i)
deriving instance (ConstraintX Nominal phase i m, ConstraintX Eq phase i m, Eq i, Nominal i, Eq m, Nominal m) => Eq (Module phase i m)
deriving instance (ConstraintX NominalShow phase i m, ConstraintX Show phase i m, Show i, NominalShow i, Show m, NominalShow m) => Show (Module phase i m)




data ModuleHeader i = ModuleHeader i (Tinydoc i) deriving (Show, Eq)


-- I need to make this extensible
-- Once I do that, I need to go to Parser.hs and fix typeDefinitionP
data Declaration phase i m = ValueDefinitionX (XValueDefinition phase i m) i (ExpressionX phase i m) 
                           | ValueDefinitionAnnotatedX (XValueDefinitionAnnotated phase i m) i (ExpressionX phase i m) (ExpressionX phase i m)
                           | TypeDefinitionX (XTypeDefinition phase i m) i (ExpressionX phase i m) [(XConstructorDefinition phase i m, i, ExpressionX phase i m)]
                         
deriving instance (ConstraintX Generic phase i m) => Generic (Declaration phase i m)
deriving instance (ConstraintX Nominal phase i m, ConstraintX Eq phase i m, Eq i, Nominal i, Eq m, Nominal m) => Eq (Declaration phase i m)
deriving instance (ConstraintX NominalShow phase i m, ConstraintX Show phase i m, Show i, NominalShow i, Show m, NominalShow m) => Show (Declaration phase i m)
deriving instance (ConstraintX Nominal phase i m, Nominal i, Nominal m) => Nominal (Declaration phase i m)
deriving instance (ConstraintX NominalShow phase i m, NominalShow i, NominalShow m) => NominalShow (Declaration phase i m)
deriving instance (ConstraintX NominalSupport phase i m, NominalSupport i, NominalSupport m) => NominalSupport (Declaration phase i m)




data ExpressionX phase i m = FirstExpressionX               (XFirstExpression phase i m)               (ExpressionX phase i m) 
                           | ParenthesizedExpressionX       (XParenthesizedExpression phase i m)       (ExpressionX phase i m) 
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

deriving instance (ConstraintX Generic phase i m) => Generic (ExpressionX phase i m)
deriving instance (ConstraintX Nominal phase i m, ConstraintX Eq phase i m, Eq i, Nominal i, Eq m, Nominal m) => Eq (ExpressionX phase i m)
deriving instance (ConstraintX NominalShow phase i m, ConstraintX Show phase i m, Show i, NominalShow i, Show m, NominalShow m) => Show (ExpressionX phase i m)
deriving instance (ConstraintX Nominal phase i m, Nominal i, Nominal m) => Nominal (ExpressionX phase i m)
deriving instance (ConstraintX NominalShow phase i m, NominalShow i, NominalShow m) => NominalShow (ExpressionX phase i m)
deriving instance (ConstraintX NominalSupport phase i m, NominalSupport i, NominalSupport m) => NominalSupport (ExpressionX phase i m)



data TelescopeX phase i m = Scope (ExpressionX phase i m) (Bind (Maybe (Binder i)) (TelescopeX phase i m)) 
                          | Pi    (ExpressionX phase i m) (Bind (Maybe (Binder i)) (ExpressionX phase i m)) 


deriving instance (ConstraintX Generic phase i m) => Generic (TelescopeX phase i m)
deriving instance (ConstraintX Nominal phase i m, ConstraintX Eq phase i m, Eq i, Nominal i, Eq m, Nominal m) => Eq (TelescopeX phase i m)
deriving instance (ConstraintX NominalShow phase i m, ConstraintX Show phase i m, Show i, NominalShow i, Show m, NominalShow m) => Show (TelescopeX phase i m)
deriving instance (ConstraintX Nominal phase i m, Nominal i, Nominal m) => Nominal (TelescopeX phase i m)
deriving instance (ConstraintX NominalShow phase i m, NominalShow i, NominalShow m) => NominalShow (TelescopeX phase i m)
deriving instance (ConstraintX NominalSupport phase i m, NominalSupport i, NominalSupport m) => NominalSupport (TelescopeX phase i m)





data FunctionLiteralX phase i m = Arg     (ExpressionX phase i m) (Bind (Binder i) (FunctionLiteralX phase i m)) 
                                | LastArg (ExpressionX phase i m) (Bind (Binder i) (ExpressionX phase i m)) 

deriving instance (ConstraintX Generic phase i m) => Generic (FunctionLiteralX phase i m)
deriving instance (ConstraintX Nominal phase i m, ConstraintX Eq phase i m, Eq i, Nominal i, Eq m, Nominal m) => Eq (FunctionLiteralX phase i m)
deriving instance (ConstraintX NominalShow phase i m, ConstraintX Show phase i m, Show i, NominalShow i, Show m, NominalShow m) => Show (FunctionLiteralX phase i m)
deriving instance (ConstraintX Nominal phase i m, Nominal i, Nominal m) => Nominal (FunctionLiteralX phase i m)
deriving instance (ConstraintX NominalShow phase i m, NominalShow i, NominalShow m) => NominalShow (FunctionLiteralX phase i m)
deriving instance (ConstraintX NominalSupport phase i m, NominalSupport i, NominalSupport m) => NominalSupport (FunctionLiteralX phase i m)





data DeclarationInfo phase i m n = Value (ExpressionX phase i m) 
                                 | ValueAndAnnotation (ExpressionX phase i m) (ExpressionX phase i m) 
                                 | TypeConstructor n (ExpressionX phase i m) 
                                 | DataConstructor Int (ExpressionX phase i m) 
type JustifiedDeclarationInfo phase ph i m n = DeclarationInfo phase i (Map.Key ph m) n


deriving instance (ConstraintX Generic phase i m) => Generic (DeclarationInfo phase i m n)
deriving instance (ConstraintX Nominal phase i m, ConstraintX Eq phase i m, Eq i, Nominal i, Eq m, Nominal m, Eq n, Nominal n) => Eq (DeclarationInfo phase i m n)
deriving instance (ConstraintX NominalShow phase i m, ConstraintX Show phase i m, Show i, NominalShow i, Show m, NominalShow m, Show n, NominalShow n) => Show (DeclarationInfo phase i m n)
deriving instance (ConstraintX Nominal phase i m, Nominal i, Nominal m, Nominal n) => Nominal (DeclarationInfo phase i m n)
deriving instance (ConstraintX NominalShow phase i m, NominalShow i, NominalShow m, NominalShow n) => NominalShow (DeclarationInfo phase i m n)
deriving instance (ConstraintX NominalSupport phase i m, NominalSupport i, NominalSupport m, NominalSupport n) => NominalSupport (DeclarationInfo phase i m n)




pattern ParenthesizedExpression e = ParenthesizedExpressionX NoInfo e
pattern FirstExpression e = FirstExpressionX NoInfo e
pattern SecondExpression e = SecondExpressionX NoInfo e
pattern PairExpression e1 e2 = PairExpressionX NoInfo e1 e2
pattern TSigmaBinding e1 e2 = TSigmaBindingX NoInfo e1 e2
pattern UniverseExpression e = UniverseExpressionX NoInfo e
pattern NumberLiteral e = NumberLiteralX NoInfo e
pattern AddExpression e1 e2 = AddExpressionX NoInfo e1 e2
pattern ReferenceVariable i m = ReferenceVariableX NoInfo i m
pattern LambdaVariable e = LambdaVariableX NoInfo e
pattern FunctionLiteralExpression e = FunctionLiteralExpressionX NoInfo e
pattern FunctionApplicationExpression e args = FunctionApplicationExpressionX NoInfo e args
pattern TArrowBinding e = TArrowBindingX NoInfo e
pattern Annotation e1 e2 = AnnotationX NoInfo e1 e2
pattern NatTypeExpression = NatTypeExpressionX NoInfo

data NoInfo i p = NoInfo deriving (Show, Eq, Generic, Bindable, Nominal, NominalShow, NominalSupport)
data NotAllowed i p deriving (Show, Eq, Generic, Bindable, Nominal, NominalShow, NominalSupport)
class AstInfo phase where 
  traverseInfo :: (Applicative a) => (i1 -> a i2) -> (m1 -> a m2) -> phase i1 m1 -> a (phase i2 m2)
instance AstInfo NoInfo where 
  traverseInfo _ _ _ = pure NoInfo
instance AstInfo NotAllowed where 
  traverseInfo _ _ a = case a of { }
instance AstInfo SourcePositionW where 
  traverseInfo _ _ (Sp s) = pure (Sp s)
instance AstInfo SourcePositionPair where 
  traverseInfo _ _ (SpPair s) = pure (SpPair s)


-- different families for type functions
type family XParenthesizedExpression phase :: * -> * -> *
type family XFirstExpression phase :: * -> * -> *
type family XSecondExpression phase :: * -> * -> *
type family XPairExpression phase :: * -> * -> *
type family XTSigmaBinding phase :: * -> * -> *
type family XUniverseExpression phase :: * -> * -> *
type family XNumberLiteral phase :: * -> * -> *
type family XAddExpression phase :: * -> * -> *
type family XReferenceVariable phase :: * -> * -> *
type family XLambdaVariable phase :: * -> * -> *
type family XFunctionLiteralExpression phase :: * -> * -> *
type family XFunctionApplicationExpression phase :: * -> * -> *
type family XTArrowBinding phase :: * -> * -> *
type family XAnnotation phase :: * -> * -> *
type family XNatTypeExpression phase :: * -> * -> * 
type family XValueDefinition phase :: * -> * -> *
type family XValueDefinitionAnnotated phase :: * -> * -> *
type family XTypeDefinition phase :: * -> * -> *
type family XConstructorDefinition phase :: * -> * -> *
type family XOther phase :: * -> * -> *

-- The parsed tag represents the AST with no additional info
data Plain 
type instance XParenthesizedExpression Plain = NoInfo
type instance XFirstExpression         Plain = NoInfo
type instance XSecondExpression        Plain = NoInfo
type instance XPairExpression          Plain = NoInfo
type instance XTSigmaBinding           Plain = NoInfo
type instance XUniverseExpression      Plain = NoInfo
type instance XNumberLiteral           Plain = NoInfo
type instance XAddExpression           Plain = NoInfo
type instance XReferenceVariable       Plain = NoInfo
type instance XLambdaVariable          Plain = NoInfo
type instance XFunctionLiteralExpression Plain = NoInfo
type instance XFunctionApplicationExpression Plain = NoInfo
type instance XTArrowBinding           Plain = NoInfo
type instance XAnnotation              Plain = NoInfo
type instance XNatTypeExpression       Plain = NoInfo
type instance XValueDefinition         Plain = NoInfo
type instance XValueDefinitionAnnotated Plain = NoInfo
type instance XTypeDefinition          Plain = NoInfo
type instance XConstructorDefinition   Plain = NoInfo
type instance XOther                   Plain = NotAllowed


-- The parsed tag represents the AST with no additional info
data Parsed 
type instance XParenthesizedExpression Parsed = SourcePositionW
type instance XFirstExpression         Parsed = SourcePositionW
type instance XSecondExpression        Parsed = SourcePositionW
type instance XPairExpression          Parsed = SourcePositionW
type instance XTSigmaBinding           Parsed = SourcePositionW
type instance XUniverseExpression      Parsed = SourcePositionW
type instance XNumberLiteral           Parsed = SourcePositionW
type instance XAddExpression           Parsed = SourcePositionW
type instance XReferenceVariable       Parsed = SourcePositionW
type instance XLambdaVariable          Parsed = SourcePositionW
type instance XFunctionLiteralExpression Parsed = SourcePositionW
type instance XFunctionApplicationExpression Parsed = SourcePositionW
type instance XTArrowBinding           Parsed = SourcePositionW
type instance XAnnotation              Parsed = SourcePositionW
type instance XNatTypeExpression       Parsed = SourcePositionW
type instance XValueDefinition         Parsed = SourcePositionW
type instance XValueDefinitionAnnotated Parsed = SourcePositionPair
type instance XTypeDefinition          Parsed = SourcePositionW
type instance XConstructorDefinition   Parsed = SourcePositionW
type instance XOther                   Parsed = NotAllowed


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
    constraint (XValueDefinition phase i m), 
    constraint (XValueDefinitionAnnotated phase i m), 
    constraint (XTypeDefinition phase i m), 
    constraint (XConstructorDefinition phase i m), 
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
    Generic (XValueDefinition phase i m), 
    Generic (XValueDefinitionAnnotated phase i m), 
    Generic (XTypeDefinition phase i m), 
    Generic (XConstructorDefinition phase i m), 
    Generic (XOther phase i m), 
    
    AstInfo (XParenthesizedExpression phase), 
    AstInfo (XFirstExpression phase), 
    AstInfo (XSecondExpression phase), 
    AstInfo (XPairExpression phase), 
    AstInfo (XTSigmaBinding phase), 
    AstInfo (XUniverseExpression phase), 
    AstInfo (XNumberLiteral phase), 
    AstInfo (XAddExpression phase), 
    AstInfo (XReferenceVariable phase), 
    AstInfo (XLambdaVariable phase), 
    AstInfo (XFunctionLiteralExpression phase), 
    AstInfo (XFunctionApplicationExpression phase), 
    AstInfo (XTArrowBinding phase), 
    AstInfo (XAnnotation phase), 
    AstInfo (XNatTypeExpression phase), 
    AstInfo (XValueDefinition phase), 
    AstInfo (XValueDefinitionAnnotated phase), 
    AstInfo (XTypeDefinition phase), 
    AstInfo (XConstructorDefinition phase), 
    AstInfo (XOther phase)
  )


data Tinydoc i = Tinydoc i deriving (Show, Read, Eq, Ord, Generic, Typeable)

data Untyped = Untyped deriving (Show, Read, Eq, Ord, NominalSupport, NominalShow, Generic, Nominal, Bindable)


type JustifiedModule           phase ph i m n = Map.Map ph m (DeclarationInfo phase i (Map.Key ph m) n)
type JustifiedExpressionX      phase ph i m   = ExpressionX phase i (Map.Key ph m)
type JustifiedTelescopeX       phase ph i m   = TelescopeX phase i (Map.Key ph m)
type JustifiedFunctionLiteralX phase ph i m   = FunctionLiteralX phase i (Map.Key ph m)




newtype SourcePositionPair i m = SpPair (SourcePosition, SourcePosition) deriving (Read, Eq, Show, NominalSupport, NominalShow, Generic, Nominal, Bindable)
newtype SourcePositionW i m = Sp { sp :: SourcePosition } deriving (Read, Eq, Show, NominalSupport, NominalShow, Generic, Nominal, Bindable)
data SourcePosition = SourcePosition {_filePath :: String, _sourceLineStart :: Int, _sourceColumnStart  :: Int, _sourceLineEnd :: Int, _sourceColumnEnd  :: Int} 
                    | Base 
                    deriving (Read, Eq, NominalSupport, NominalShow, Generic, Nominal, Bindable)
instance Show (SourcePosition) where
  --show _ = ""
  show (SourcePosition f l1 c1 l2 c2) = "(SourcePosition \"" <> f <> "\" " <> (show l1) <> " " <> (show c1) <> " " <> (show l2) <> " " <> (show c2) <> ")"
  show (Base) = "Base" 




getOutputOfScope :: (ConstraintX Nominal phase i m, Nominal m, Nominal i) => TelescopeX phase i m -> ExpressionX phase i m
getOutputOfScope (Scope _ (_ :. scope) ) = getOutputOfScope scope
getOutputOfScope (Pi    _ (_ :. e) ) = e

getBodyOfFlit :: (ConstraintX Nominal phase i m, Nominal m, Nominal i) => FunctionLiteralX phase i m -> ExpressionX phase i m
getBodyOfFlit (Arg     _ (_ :. scope)) = getBodyOfFlit scope
getBodyOfFlit (LastArg _ (_ :. e)) = e




substExpr :: (ConstraintX Nominal phase i m, Nominal i, Nominal m) => Atom -> ExpressionX phase i m -> ExpressionX phase i m -> ExpressionX phase i m
substExpr lookingFor substWith = subst 
  where subst (ParenthesizedExpressionX inf e) = ParenthesizedExpressionX inf (subst e)
        subst (FirstExpressionX inf e) = FirstExpressionX inf (subst e)
        subst (SecondExpressionX inf e) = SecondExpressionX inf (subst e)
        subst (PairExpressionX inf e1 e2) = PairExpressionX inf (subst e1) (subst e2)
        subst (TSigmaBindingX inf e1 (a :. e2)) = TSigmaBindingX inf (subst e1) (a :. (subst e2))
        subst (UniverseExpressionX inf i) = UniverseExpressionX inf i
        subst (NumberLiteralX inf i) = NumberLiteralX inf i
        subst (AddExpressionX inf e1 e2) = AddExpressionX inf (subst e1) (subst e2)
        subst (ReferenceVariableX inf i m) = ReferenceVariableX inf i m 
        subst (LambdaVariableX inf (a, i)) = if a == lookingFor then substWith else LambdaVariableX inf (a, i)
        subst (FunctionApplicationExpressionX inf func args) = FunctionApplicationExpressionX inf (subst func) (fmap subst args)
        subst (TArrowBindingX inf scope) = TArrowBindingX inf (substTelescope lookingFor substWith scope)
        subst (AnnotationX inf e1 e2) = AnnotationX inf (subst e1) (subst e2)
        subst (NatTypeExpressionX inf) = NatTypeExpressionX inf 
        subst (FunctionLiteralExpressionX inf flit) = FunctionLiteralExpressionX inf (substFlit lookingFor substWith flit)

substTelescope :: (ConstraintX Nominal phase i m, Nominal i, Nominal m) => Atom -> ExpressionX phase i m -> TelescopeX phase i m -> TelescopeX phase i m
substTelescope lookingFor substWith = subst 
  where substE = substExpr lookingFor substWith
        subst (Scope e1 (a :. scope)) = Scope (substE e1) (a :. subst scope)
        subst (Pi    e1 (a :. e2   )) = Pi (substE e1) (a :. substE e2)
substFlit :: (ConstraintX Nominal phase i m, Nominal i, Nominal m) => Atom -> ExpressionX phase i m -> FunctionLiteralX phase i m -> FunctionLiteralX phase i m
substFlit lookingFor substWith = subst
  where substE = substExpr lookingFor substWith
        subst (Arg     e1 (a :. scope)) = Arg     (substE e1) (a :. subst scope)
        subst (LastArg e1 (a :. e2   )) = LastArg (substE e1) (a :. substE e2)

liftA4 :: Applicative f => (a1 -> b1 -> c -> a2 -> b2) -> f a1 -> f b1 -> f c -> f a2 -> f b2
liftA4 f a b c d = liftA3 f a b c <*> d

traverseExpr :: forall i1 m1 i2 m2 a phase. (ConstraintX Bindable phase i1 m1, ConstraintX Bindable phase i2 m2, Bindable i1, Bindable i2, Bindable m1, Bindable m2, Applicative a) => (i1 -> a i2) -> (m1 -> a m2) -> ExpressionX phase i1 m1 -> a (ExpressionX phase i2 m2)
traverseExpr fi fm = me
  where me :: ExpressionX phase i1 m1 -> a (ExpressionX phase i2 m2)
        me (ParenthesizedExpressionX inf e) = liftA2 ParenthesizedExpressionX (tInf inf) (me e)
        me (FirstExpressionX inf e)         = liftA2 FirstExpressionX (tInf inf) (me e)
        me (SecondExpressionX inf e)        = liftA2 SecondExpressionX (tInf inf) (me e)
        me (PairExpressionX inf e1 e2)      = liftA3 PairExpressionX (tInf inf) (me e1) (me e2)
        me (TSigmaBindingX inf e1 (Just (a, NoBind i) :. e2)) = liftA3 TSigmaBindingX (tInf inf) (me e1) (liftA2 abst (liftA (\x -> Just (a, NoBind x)) (fi i)) (me e2))
        me (TSigmaBindingX inf e1 (Nothing :. e2)) = liftA3 TSigmaBindingX (tInf inf) (me e1) (liftA2 abst (pure Nothing) (me e2))
        me (AddExpressionX inf e1 e2)       = liftA3 AddExpressionX (tInf inf) (me e1) (me e2)
        me (ReferenceVariableX inf i m)     = liftA3 ReferenceVariableX (tInf inf) (fi i) (fm m)
        me (LambdaVariableX inf (a, i))     = liftA2 LambdaVariableX (tInf inf) ((\x -> (a, x)) <$> fi i)
        me (FunctionLiteralExpressionX inf flit) = liftA2 FunctionLiteralExpressionX (tInf inf) (traverseFunctionLiteral fi fm flit)
        me (FunctionApplicationExpressionX inf func args) = liftA3 FunctionApplicationExpressionX (tInf inf) (me func) (traverse me args)
        me (TArrowBindingX inf telescope)   = liftA2 TArrowBindingX (tInf inf) (traverseTelescope fi fm telescope)
        me (AnnotationX inf e1 e2)          = liftA3 AnnotationX (tInf inf) (me e1) (me e2)
        me (UniverseExpressionX inf i)      = liftA2 UniverseExpressionX (tInf inf) (pure i)
        me (NumberLiteralX inf i)           = liftA2 NumberLiteralX (tInf (inf :: XNumberLiteral phase i1 m1)) (pure i)
        me (NatTypeExpressionX inf)         = liftA NatTypeExpressionX ((tInf (inf :: (XNatTypeExpression phase i1 m1))) :: a (XNatTypeExpression phase i2 m2))
        tInf :: (AstInfo inf) => inf i1 m1 -> a (inf i2 m2)
        tInf = traverseInfo fi fm

traverseTelescope :: forall i1 m1 i2 m2 a phase. (ConstraintX Bindable phase i1 m1, ConstraintX Bindable phase i2 m2, Bindable i1, Bindable i2, Bindable m1, Bindable m2, Applicative a) => (i1 -> a i2) -> (m1 -> a m2) -> TelescopeX phase i1 m1 -> a (TelescopeX phase i2 m2)
traverseTelescope fi fm = te
  where te (Scope e ((Just (a, NoBind i)) :. scope))   = liftA2 Scope (me e) (liftA2 abst (liftA (filler a) (fi i)) (te scope))
        te (Scope e ((Nothing                       :. scope)))  = liftA2 Scope (me e) (liftA2 abst (pure Nothing)            (te scope))
        te (Pi    e ((Just (a, NoBind i)) :. eBound))  = liftA2 Pi    (me e) (liftA2 abst (liftA (filler a) (fi i)) (me eBound))
        te (Pi    e ((Nothing                       :. eBound))) = liftA2 Pi    (me e) (liftA2 abst (pure Nothing)            (me eBound))
        filler a i = Just (a, NoBind i)
        me = traverseExpr fi fm


traverseFunctionLiteral :: forall i1 m1 i2 m2 a phase. (ConstraintX Nominal phase i1 m1, ConstraintX Nominal phase i2 m2, ConstraintX Bindable phase i1 m1, ConstraintX Bindable phase i2 m2, Bindable i1, Bindable i2, Bindable m1, Bindable m2, Applicative a) => (i1 -> a i2) -> (m1 -> a m2) -> FunctionLiteralX phase i1 m1 -> a (FunctionLiteralX phase i2 m2)
traverseFunctionLiteral fi fm = fe
  where fe :: FunctionLiteralX phase i1 m1 -> a (FunctionLiteralX phase i2 m2)
        fe (Arg     e (((a, NoBind i)) :. scope))   = liftA2 Arg     (me e) (liftA2 abst (liftA (binder a) (fi i)) (fe scope))
        fe (LastArg e (((a, NoBind i)) :. eBound))  = liftA2 LastArg (me e) (liftA2 abst (liftA (binder a) (fi i)) (me eBound))
        binder a i = (a, NoBind i)
        me :: ExpressionX phase i1 m1 -> a (ExpressionX phase i2 m2)
        me = traverseExpr fi fm


traverseExpr1 fm = traverseExpr (fmap pure id) (fm)

forExpr1 = flip traverseExpr1


mapExpr fi fm e = runIdentity $ traverseExpr fim fmm e
  where fim = fmap pure fi
        fmm = fmap pure fm

mapDeclarationInfo f (Value e) = Value (f e)
mapDeclarationInfo f (ValueAndAnnotation e t) = ValueAndAnnotation (f e) (f t)
mapDeclarationInfo f (TypeConstructor i e) = TypeConstructor i (f e) 
mapDeclarationInfo f (DataConstructor i e) = DataConstructor i (f e)


toPlain :: forall i m. (ConstraintX Bindable Parsed i m, ConstraintX Bindable Plain i m, Bindable i, Bindable m) => ExpressionX Parsed i m -> ExpressionX Plain i m
toPlain (ParenthesizedExpressionX _ e) = ParenthesizedExpressionX NoInfo (toPlain e)
toPlain (FirstExpressionX _ e) = FirstExpressionX NoInfo (toPlain e)
toPlain (SecondExpressionX _ e) = SecondExpressionX NoInfo (toPlain e)
toPlain (PairExpressionX _ e1 e2) = PairExpressionX NoInfo (toPlain e1) (toPlain e2)
toPlain (TSigmaBindingX _ e1 (a :. e2)) = TSigmaBindingX NoInfo (toPlain e1) (a :. (toPlain e2))
toPlain (UniverseExpressionX _ i) = UniverseExpressionX NoInfo i
toPlain (NumberLiteralX _ i) = NumberLiteralX NoInfo i
toPlain (AddExpressionX _ e1 e2) = AddExpressionX NoInfo (toPlain e1) (toPlain e2)
toPlain (ReferenceVariableX _ i m) = ReferenceVariableX NoInfo i m 
toPlain (LambdaVariableX _ (a, i)) = LambdaVariableX NoInfo (a, i) 
toPlain (FunctionApplicationExpressionX _ func args) = FunctionApplicationExpressionX NoInfo (toPlain func) (fmap toPlain args)
toPlain (TArrowBindingX _ scope) = TArrowBindingX NoInfo (toPlainTelescope scope)
toPlain (AnnotationX _ e1 e2) = AnnotationX NoInfo (toPlain e1) (toPlain e2)
toPlain (NatTypeExpressionX _) = NatTypeExpressionX NoInfo 
toPlain (FunctionLiteralExpressionX _ flit) = FunctionLiteralExpressionX NoInfo (toPlainFlit flit)



toPlainTelescope :: forall i m. (ConstraintX Bindable Parsed i m, ConstraintX Bindable Plain i m, Bindable i, Bindable m) => TelescopeX Parsed i m -> TelescopeX Plain i m
toPlainTelescope (Scope e1 (a :. scope)) = Scope (toPlain e1) (a :. toPlainTelescope scope)
toPlainTelescope (Pi    e1 (a :. e2   )) = Pi (toPlain e1) (a :. toPlain e2)
        
toPlainFlit :: forall i m. (ConstraintX Bindable Parsed i m, ConstraintX Bindable Plain i m, Bindable i, Bindable m) => FunctionLiteralX Parsed i m -> FunctionLiteralX Plain i m
toPlainFlit (Arg     e1 (a :. scope)) = Arg     (toPlain e1) (a :. toPlainFlit scope)
toPlainFlit (LastArg e1 (a :. e2   )) = LastArg (toPlain e1) (a :. toPlain e2)

toPlainDecl = mapDeclarationInfo toPlain

plainToNormalizedPlainDecl :: forall i m n. (ConstraintX Bindable Plain i m, ConstraintX Bindable Plain () m, Bindable i, Bindable m) => DeclarationInfo Plain i m n -> DeclarationInfo Plain () m n
plainToNormalizedPlainDecl (Value i) = Value (plainToNormalizedPlainExpr i) 
plainToNormalizedPlainDecl (ValueAndAnnotation exp1 exp2) = ValueAndAnnotation (plainToNormalizedPlainExpr exp1) (plainToNormalizedPlainExpr exp2)
plainToNormalizedPlainDecl (TypeConstructor n exp) = TypeConstructor n (plainToNormalizedPlainExpr exp)
plainToNormalizedPlainDecl (DataConstructor i exp) = DataConstructor i (plainToNormalizedPlainExpr exp)

plainToNormalizedPlainExpr :: forall i m. (ConstraintX Bindable Plain i m, ConstraintX Bindable Plain () m, Bindable i, Bindable m) => ExpressionX Plain i m -> ExpressionX Plain () m
plainToNormalizedPlainExpr = mapExpr (const ()) id

parsedToNormalizedPlain :: forall i m. (ConstraintX Bindable Parsed i m, ConstraintX Bindable Parsed () m, ConstraintX Bindable Plain () m, Bindable i, Bindable m) => ExpressionX Parsed i m -> ExpressionX Plain () m
parsedToNormalizedPlain = toPlain . mapExpr (const ()) id


normalizeDeclarationInfoMetadata :: forall i m n. (ConstraintX Bindable Parsed i m, ConstraintX Bindable Parsed () m, ConstraintX Bindable Plain () m, Bindable n, Bindable i, Bindable m) => (DeclarationInfo Parsed i m n) -> (DeclarationInfo Plain () m n)
normalizeDeclarationInfoMetadata = mapDeclarationInfo parsedToNormalizedPlain

getValueFromDeclarationInfo (Value v) = v 
getValueFromDeclarationInfo (ValueAndAnnotation v _) = v 
getValueFromDeclarationInfo (TypeConstructor _ _) = error "Expected value, got constructor!" 
getValueFromDeclarationInfo (DataConstructor _ _) = error "Expected value, got constructor!" 
