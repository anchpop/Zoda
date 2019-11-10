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


-- |Semantic values contain terms which have evaluated to a constructor which does not need to be 
-- reduced further. So for instance, Lam and Pair may contain computation further inside the term 
-- but at least the outermost constructor is stable and fully evaluated. 
data Semantic t p m i =
      LamSem (Clos t p m i)
    | NeutralSem {tpNeutral   :: Semantic t p m i,  -- ^This should be the type of the neutral term
                  termNeutral :: Ne       t p m i}  -- ^This should be the neutral term iteslf
    | NatTypeSem
    | NatValueSem Integer
    | AddSem (Semantic t p m i) (Semantic t p m i)
    | PiTypeSem (Semantic t p m i) (Clos t p m i)
    | SigTypeSem (Semantic t p m i) (Clos t p m i) 
    | PairSem (Semantic t p m i) (Semantic t p m i)
    | UniSem UniLevel
    deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal)

-- |We also have to consider the case that something wants to reduce further before becoming a 
-- value but cannot because its blocked on something. These are called neutral terms. The 
-- canonical example of a neutral term is just a variable x. It's not yet a value but there's 
-- no way to convert it to a value since we have no information on what x is yet. Similarly, if 
-- we have some neutral term and we apply Fst to it it's clearly still not a value but we don't 
-- have any way of reducing it further so what's there to do. 
data Ne t p m i =
      VarSem Atom
    | ApSem (Ne t p m i) (Nf t p m i)
    | FstSem (Ne t p m i)
    | SndSem (Ne t p m i)
    deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal)

-- |nf is a special class of values coming from the style of NbE we use. It associates a type 
-- with a value so that later during quotation we can eta expand it appropriately
data Nf t p m i =
  Normal {tpNf :: Semantic t p m i, termNf :: Semantic t p m i}
  deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal)

type UniLevel = Integer

type SurfaceEnv t p m i = [(Atom, Expression t p m i)]

type SemanticEnv t p m i = [(Atom, Semantic t p m i)]
data Clos t p m i = Clos {termClos :: (Bind (i, (Atom, p)) (Expression t p m i)), envClos :: (SemanticEnv t p m i)}
  deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal)


makeLenses ''Module
makeLenses ''ModuleHeader
makeLenses ''Declaration
makeLenses ''Expression
makeLenses ''Tinydoc

mapExpr1 :: (Bindable t, Bindable p, Bindable i1, Bindable i2, Nominal m ) => (i1 -> i2) -> Expression t p m i1 -> Expression t p m i2
mapExpr1 f (ParenthesizedExpression e t p)               = ParenthesizedExpression (mapExpr1 f e) t p
mapExpr1 f (FirstExpression e t p)                       = FirstExpression (mapExpr1 f e) t p
mapExpr1 f (SecondExpression e t p)                      = SecondExpression (mapExpr1 f e) t p
mapExpr1 f (PairExpression e1 e2 t p)                    = PairExpression (mapExpr1 f e1) (mapExpr1 f e2) t p
mapExpr1 f (TSigmaBinding e1 (a :. e2) t p)              = TSigmaBinding (mapExpr1 f e1) ((Data.Bifunctor.first f a) :. (mapExpr1 f e2)) t p
mapExpr1 f (UniverseExpression i t p)                    = UniverseExpression i t p
mapExpr1 f (NumberLiteral i1 i2 t p)                     = NumberLiteral i1 i2 t p
mapExpr1 f (AddExpression e1 e2 t p)                     = AddExpression (mapExpr1 f e1) (mapExpr1 f e2) t p
mapExpr1 f (ReferenceVariable i m t p)                   = ReferenceVariable (f i) m t p 
mapExpr1 f (LambdaVariable (i, a) t p)                   = LambdaVariable ((f i), a) t p
mapExpr1 f (FunctionLiteralExpression (a :. e) t p)      = FunctionLiteralExpression ((fmap (Data.Bifunctor.first f) a) :. (mapExpr1 f e)) t p
mapExpr1 f (FunctionApplicationExpression func args t p) = FunctionApplicationExpression (mapExpr1 f func) (fmap (mapExpr1 f) args) t p
mapExpr1 f (TArrowNonbinding e1 e2 t p)                  = TArrowNonbinding (mapExpr1 f e1) (mapExpr1 f e2) t p
mapExpr1 f (TArrowBinding e1 (a :. e2) t p)              = TArrowBinding (mapExpr1 f e1) ((Data.Bifunctor.first f a) :. (mapExpr1 f e2)) t p
mapExpr1 f (Annotation e1 e2 t p)                        = Annotation (mapExpr1 f e1) (mapExpr1 f e2) t p
mapExpr1 f (NatTypeExpression t p)                       = NatTypeExpression t p


mapExpr2 :: (Bindable t, Bindable p, Bindable i, Nominal m1, Nominal m2) => (m1 -> m2) -> Expression t p m1 i -> Expression t p m2 i
mapExpr2 f (ParenthesizedExpression e t p)               = ParenthesizedExpression (mapExpr2 f e) t p
mapExpr2 f (FirstExpression e t p)                       = FirstExpression (mapExpr2 f e) t p
mapExpr2 f (SecondExpression e t p)                      = SecondExpression (mapExpr2 f e) t p
mapExpr2 f (PairExpression e1 e2 t p)                    = PairExpression (mapExpr2 f e1) (mapExpr2 f e2) t p
mapExpr2 f (TSigmaBinding e1 (a :. e2) t p)              = TSigmaBinding (mapExpr2 f e1) (a :. (mapExpr2 f e2)) t p
mapExpr2 f (UniverseExpression i t p)                    = UniverseExpression i t p
mapExpr2 f (NumberLiteral i1 i2 t p)                     = NumberLiteral i1 i2 t p
mapExpr2 f (AddExpression e1 e2 t p)                     = AddExpression (mapExpr2 f e1) (mapExpr2 f e2) t p
mapExpr2 f (ReferenceVariable i m t p)                   = ReferenceVariable i (f m) t p 
mapExpr2 f (LambdaVariable i t p)                        = LambdaVariable i t p
mapExpr2 f (FunctionLiteralExpression (a :. e) t p)      = FunctionLiteralExpression (a :. (mapExpr2 f e)) t p
mapExpr2 f (FunctionApplicationExpression func args t p) = FunctionApplicationExpression (mapExpr2 f func) (fmap (mapExpr2 f) args) t p
mapExpr2 f (TArrowNonbinding e1 e2 t p)                  = TArrowNonbinding (mapExpr2 f e1) (mapExpr2 f e2) t p
mapExpr2 f (TArrowBinding e1 (a :. e2) t p)              = TArrowBinding (mapExpr2 f e1) (a :. (mapExpr2 f e2)) t p
mapExpr2 f (Annotation e1 e2 t p)                        = Annotation (mapExpr2 f e1) (mapExpr2 f e2) t p
mapExpr2 f (NatTypeExpression t p)                       = NatTypeExpression t p


copyIdentifierOntoMetadata :: (Bindable t, Bindable p, Bindable i, Nominal m) => Expression t p m i -> Expression t p (i, p) i
copyIdentifierOntoMetadata (ParenthesizedExpression e t p)               = ParenthesizedExpression (copyIdentifierOntoMetadata e) t p
copyIdentifierOntoMetadata (FirstExpression e t p)                       = FirstExpression (copyIdentifierOntoMetadata e) t p
copyIdentifierOntoMetadata (SecondExpression e t p)                      = SecondExpression (copyIdentifierOntoMetadata e) t p
copyIdentifierOntoMetadata (PairExpression e1 e2 t p)                    = PairExpression (copyIdentifierOntoMetadata e1) (copyIdentifierOntoMetadata e2) t p
copyIdentifierOntoMetadata (TSigmaBinding e1 (a :. e2) t p)              = TSigmaBinding (copyIdentifierOntoMetadata e1) (a :. (copyIdentifierOntoMetadata e2)) t p
copyIdentifierOntoMetadata (UniverseExpression i t p)                    = UniverseExpression i t p
copyIdentifierOntoMetadata (NumberLiteral i1 i2 t p)                     = NumberLiteral i1 i2 t p
copyIdentifierOntoMetadata (AddExpression e1 e2 t p)                     = AddExpression (copyIdentifierOntoMetadata e1) (copyIdentifierOntoMetadata e2) t p
copyIdentifierOntoMetadata (ReferenceVariable i m t p)                   = ReferenceVariable i (i, p) t p 
copyIdentifierOntoMetadata (LambdaVariable i t p)                        = LambdaVariable i t p
copyIdentifierOntoMetadata (FunctionLiteralExpression (a :. e) t p)      = FunctionLiteralExpression (a :. (copyIdentifierOntoMetadata e)) t p
copyIdentifierOntoMetadata (FunctionApplicationExpression func args t p) = FunctionApplicationExpression (copyIdentifierOntoMetadata func) (fmap (copyIdentifierOntoMetadata) args) t p
copyIdentifierOntoMetadata (TArrowNonbinding e1 e2 t p)                  = TArrowNonbinding (copyIdentifierOntoMetadata e1) (copyIdentifierOntoMetadata e2) t p
copyIdentifierOntoMetadata (TArrowBinding e1 (a :. e2) t p)              = TArrowBinding (copyIdentifierOntoMetadata e1) (a :. (copyIdentifierOntoMetadata e2)) t p
copyIdentifierOntoMetadata (Annotation e1 e2 t p)                        = Annotation (copyIdentifierOntoMetadata e1) (copyIdentifierOntoMetadata e2) t p
copyIdentifierOntoMetadata (NatTypeExpression t p)                       = NatTypeExpression t p


traverseExpr2 :: (Bindable t, Bindable p, Bindable i, Nominal m1, Nominal m2, Monad mo) => (m1 -> mo m2) -> Expression t p m1 i -> mo (Expression t p m2 i)
traverseExpr2 f (ParenthesizedExpression e t p)               = do
  e' <- traverseExpr2 f e
  pure $ ParenthesizedExpression (e') t p
traverseExpr2 f (FirstExpression e t p)                       = do
  e' <- traverseExpr2 f e
  pure $ FirstExpression e' t p
traverseExpr2 f (SecondExpression e t p)                      = do 
  e' <- traverseExpr2 f e
  pure $ SecondExpression e' t p
traverseExpr2 f (PairExpression e1 e2 t p)                    = do
  e1' <- traverseExpr2 f e1
  e2' <- traverseExpr2 f e2
  pure $ PairExpression e1' e2' t p
traverseExpr2 f (TSigmaBinding e1 (a :. e2) t p)              = do 
  e1' <- traverseExpr2 f e1
  e2' <- traverseExpr2 f e2
  pure $ TSigmaBinding e1' (a :. e2') t p
traverseExpr2 f (UniverseExpression i t p)                    = pure $ UniverseExpression i t p
traverseExpr2 f (NumberLiteral i1 i2 t p)                     = pure $ NumberLiteral i1 i2 t p
traverseExpr2 f (AddExpression e1 e2 t p)                     = do
  e1' <- traverseExpr2 f e1
  e2' <- traverseExpr2 f e2
  pure $ AddExpression e1' e2' t p
traverseExpr2 f (ReferenceVariable i m t p)                   = do
  m' <- f m
  pure $ ReferenceVariable i m' t p 
traverseExpr2 f (LambdaVariable i t p)                        = pure $ LambdaVariable i t p
traverseExpr2 f (FunctionLiteralExpression (a :. e) t p)      = do
  e' <- traverseExpr2 f e
  pure $ FunctionLiteralExpression (a :. e') t p
traverseExpr2 f (FunctionApplicationExpression func args t p) = do
  func' <- traverseExpr2 f func
  args' <- traverse (traverseExpr2 f) args 
  pure $ FunctionApplicationExpression func' args' t p
traverseExpr2 f (TArrowNonbinding e1 e2 t p)                  = do
  e1' <- traverseExpr2 f e1
  e2' <- traverseExpr2 f e2
  pure $ TArrowNonbinding e1' e2' t p
traverseExpr2 f (TArrowBinding e1 (a :. e2) t p)              = do 
  e1' <- traverseExpr2 f e1
  e2' <- traverseExpr2 f e2
  pure $ TArrowBinding e1' (a :. e2') t p
traverseExpr2 f (Annotation e1 e2 t p)                        = do 
  e1' <- traverseExpr2 f e1 
  e2' <- traverseExpr2 f e2
  pure $ Annotation e1' e2' t p
traverseExpr2 f (NatTypeExpression t p)                       = pure $ NatTypeExpression t p

forExpr2 x y = traverseExpr2 y x


mapExpr3 :: (Bindable t, Bindable p1, Bindable p2, Bindable i, Nominal m) => (p1 -> p2) -> Expression t p1 m i -> Expression t p2 m i
mapExpr3 f (ParenthesizedExpression e t p)               = ParenthesizedExpression (mapExpr3 f e) t (f p)
mapExpr3 f (FirstExpression e t p)                       = FirstExpression (mapExpr3 f e) t (f p)
mapExpr3 f (SecondExpression e t p)                      = SecondExpression (mapExpr3 f e) t (f p)
mapExpr3 f (PairExpression e1 e2 t p)                    = PairExpression (mapExpr3 f e1) (mapExpr3 f e2) t (f p)
mapExpr3 f (TSigmaBinding e1 (a :. e2) t p)              = TSigmaBinding (mapExpr3 f e1) ((Data.Bifunctor.second (Data.Bifunctor.second f) a) :. (mapExpr3 f e2)) t (f p)
mapExpr3 f (UniverseExpression i t p)                    = UniverseExpression i t (f p)
mapExpr3 f (NumberLiteral i1 i2 t p)                     = NumberLiteral i1 i2 t (f p)
mapExpr3 f (AddExpression e1 e2 t p)                     = AddExpression (mapExpr3 f e1) (mapExpr3 f e2) t (f p)
mapExpr3 f (ReferenceVariable i m t p)                   = ReferenceVariable i m t (f p) 
mapExpr3 f (LambdaVariable i t p)                        = LambdaVariable i t (f p)
mapExpr3 f (FunctionLiteralExpression (a :. e) t p)      = FunctionLiteralExpression ((fmap (Data.Bifunctor.second (Data.Bifunctor.second f)) a) :. (mapExpr3 f e)) t (f p)
mapExpr3 f (FunctionApplicationExpression func args t p) = FunctionApplicationExpression (mapExpr3 f func) (fmap (mapExpr3 f) args) t (f p)
mapExpr3 f (TArrowNonbinding e1 e2 t p)                  = TArrowNonbinding (mapExpr3 f e1) (mapExpr3 f e2) t (f p)
mapExpr3 f (TArrowBinding e1 (a :. e2) t p)              = TArrowBinding (mapExpr3 f e1) ((Data.Bifunctor.second (Data.Bifunctor.second f) a) :. (mapExpr3 f e2)) t (f p)
mapExpr3 f (Annotation e1 e2 t p)                        = Annotation (mapExpr3 f e1) (mapExpr3 f e2) t (f p)
mapExpr3 f (NatTypeExpression t p)                       = NatTypeExpression t (f p)


mapExpr4 :: (Bindable p, Bindable i, Nominal t1, Nominal t2, Nominal m) => (t1 -> t2) -> Expression t1 p m i -> Expression t2 p m i
mapExpr4 f (ParenthesizedExpression e t p)               = ParenthesizedExpression (mapExpr4 f e) (f t) p
mapExpr4 f (FirstExpression e t p)                       = FirstExpression (mapExpr4 f e) (f t) p
mapExpr4 f (SecondExpression e t p)                      = SecondExpression (mapExpr4 f e) (f t) p
mapExpr4 f (PairExpression e1 e2 t p)                    = PairExpression (mapExpr4 f e1) (mapExpr4 f e2) (f t) p
mapExpr4 f (TSigmaBinding e1 (a :. e2) t p)              = TSigmaBinding (mapExpr4 f e1) (a :. (mapExpr4 f e2)) (f t) p
mapExpr4 f (UniverseExpression i t p)                    = UniverseExpression i (f t) p
mapExpr4 f (NumberLiteral i1 i2 t p)                     = NumberLiteral i1 i2 (f t) p
mapExpr4 f (AddExpression e1 e2 t p)                     = AddExpression (mapExpr4 f e1) (mapExpr4 f e2) (f t) p
mapExpr4 f (ReferenceVariable i m t p)                   = ReferenceVariable i m (f t) p 
mapExpr4 f (LambdaVariable i t p)                        = LambdaVariable i (f t) p
mapExpr4 f (FunctionLiteralExpression (a :. e) t p)      = FunctionLiteralExpression (a :. (mapExpr4 f e)) (f t) p
mapExpr4 f (FunctionApplicationExpression func args t p) = FunctionApplicationExpression (mapExpr4 f func) (fmap (mapExpr4 f) args) (f t) p
mapExpr4 f (TArrowNonbinding e1 e2 t p)                  = TArrowNonbinding (mapExpr4 f e1) (mapExpr4 f e2) (f t) p
mapExpr4 f (TArrowBinding e1 (a :. e2) t p)              = TArrowBinding (mapExpr4 f e1) (a :. (mapExpr4 f e2)) (f t) p
mapExpr4 f (Annotation e1 e2 t p)                        = Annotation (mapExpr4 f e1) (mapExpr4 f e2) (f t) p
mapExpr4 f (NatTypeExpression t p)                       = NatTypeExpression (f t) p

normalizeExprMetadata e = mapExpr4 (const ()) $ mapExpr3 (const ()) $ mapExpr2 (const ()) $ mapExpr1 (const ()) $ e


mapSemantic1 :: (Bindable t, Bindable i1, Bindable i2, Bindable p, Nominal m) => (i1 -> i2) -> Semantic t p m i1 -> Semantic t p m i2
mapSemantic1 f (LamSem clos)                        = LamSem (mapClos1 f clos)
mapSemantic1 f (NeutralSem typeNeutral termNeutral) = NeutralSem (mapSemantic1 f typeNeutral) (mapNe1 f termNeutral)
mapSemantic1 f (NatTypeSem)                         = NatTypeSem
mapSemantic1 f (NatValueSem i)                      = NatValueSem i
mapSemantic1 f (AddSem s1 s2)                       = AddSem (mapSemantic1 f s1) (mapSemantic1 f s2)
mapSemantic1 f (PiTypeSem s c)                      = PiTypeSem (mapSemantic1 f s) (mapClos1 f c)
mapSemantic1 f (SigTypeSem s c)                     = SigTypeSem (mapSemantic1 f s) (mapClos1 f c)
mapSemantic1 f (PairSem s1 s2)                      = PairSem (mapSemantic1 f s1) (mapSemantic1 f s2)
mapSemantic1 f (UniSem i)                           = UniSem i

mapNf1 :: (Bindable t, Bindable i1, Bindable i2, Bindable p, Nominal m) => (i1 -> i2) -> Nf t p m i1 -> Nf t p m i2
mapNf1 f (Normal tp tm) = Normal (mapSemantic1 f tp) (mapSemantic1 f tm)

mapNe1 :: (Bindable t, Bindable i1, Bindable i2, Bindable p, Nominal m) => (i1 -> i2) -> Ne t p m i1 -> Ne t p m i2
mapNe1 f (VarSem a) = VarSem a
mapNe1 f (ApSem ne nf) = ApSem (mapNe1 f ne) (mapNf1 f nf)
mapNe1 f (FstSem ne) = FstSem (mapNe1 f ne)
mapNe1 f (SndSem ne) = SndSem (mapNe1 f ne)

mapClos1 :: (Bindable t, Bindable i1, Bindable i2, Bindable p, Nominal m) => (i1 -> i2) -> Clos t p m i1 -> Clos t p m i2
mapClos1 f (Clos ((i, (atom, p)) :. surf) (semanticEnv)) = Clos ((f i, (atom, p)) :. (mapExpr1 f surf)) (fmap (Data.Bifunctor.second (mapSemantic1 f)) semanticEnv)



mapSemantic2 :: (Bindable t, Bindable p, Bindable i, Nominal m1, Nominal m2) => (m1 -> m2) -> Semantic t p m1 i -> Semantic t p m2 i
mapSemantic2 f (LamSem clos)                        = LamSem (mapClos2 f clos)
mapSemantic2 f (NeutralSem typeNeutral termNeutral) = NeutralSem (mapSemantic2 f typeNeutral) (mapNe2 f termNeutral)
mapSemantic2 f (NatTypeSem)                         = NatTypeSem
mapSemantic2 f (NatValueSem i)                      = NatValueSem i
mapSemantic2 f (AddSem s1 s2)                       = AddSem (mapSemantic2 f s1) (mapSemantic2 f s2)
mapSemantic2 f (PiTypeSem s c)                      = PiTypeSem (mapSemantic2 f s) (mapClos2 f c)
mapSemantic2 f (SigTypeSem s c)                     = SigTypeSem (mapSemantic2 f s) (mapClos2 f c)
mapSemantic2 f (PairSem s1 s2)                      = PairSem (mapSemantic2 f s1) (mapSemantic2 f s2)
mapSemantic2 f (UniSem i)                           = UniSem i

mapNf2 :: (Bindable t, Bindable p, Bindable i, Nominal m1, Nominal m2) => (m1 -> m2) -> Nf t p m1 i -> Nf t p m2 i
mapNf2 f (Normal tp tm) = Normal (mapSemantic2 f tp) (mapSemantic2 f tm)

mapNe2 :: (Bindable t, Bindable p, Bindable i, Nominal m1, Nominal m2) => (m1 -> m2) -> Ne t p m1 i -> Ne t p m2 i
mapNe2 f (VarSem a) = VarSem a
mapNe2 f (ApSem ne nf) = (ApSem (mapNe2 f ne) (mapNf2 f nf))
mapNe2 f (FstSem ne) = FstSem (mapNe2 f ne)
mapNe2 f (SndSem ne) = SndSem (mapNe2 f ne)

mapClos2 :: (Bindable t, Bindable p, Bindable i, Nominal m1, Nominal m2) => (m1 -> m2) -> Clos t p m1 i -> Clos t p m2 i
mapClos2 f (Clos ((i, (atom, p)) :. surf) semanticEnv) = Clos ((i, (atom, p)) :. mapExpr2 f surf) (fmap (\(a, b) -> (a, mapSemantic2 f b)) semanticEnv)


mapSemantic3 ::(Bindable t, Bindable p1, Bindable p2, Bindable i, Nominal m) => (p1 -> p2) -> Semantic t p1 m i -> Semantic t p2 m i
mapSemantic3 f (LamSem clos)                        = LamSem (mapClos3 f clos)
mapSemantic3 f (NeutralSem typeNeutral termNeutral) = NeutralSem (mapSemantic3 f typeNeutral) (mapNe3 f termNeutral)
mapSemantic3 f (NatTypeSem)                         = NatTypeSem
mapSemantic3 f (NatValueSem i)                      = NatValueSem i
mapSemantic3 f (AddSem s1 s2)                       = AddSem (mapSemantic3 f s1) (mapSemantic3 f s2)
mapSemantic3 f (PiTypeSem s c)                      = PiTypeSem (mapSemantic3 f s) (mapClos3 f c)
mapSemantic3 f (SigTypeSem s c)                     = SigTypeSem (mapSemantic3 f s) (mapClos3 f c)
mapSemantic3 f (PairSem s1 s2)                      = PairSem (mapSemantic3 f s1) (mapSemantic3 f s2)
mapSemantic3 f (UniSem i)                           = UniSem i

mapNf3 ::(Bindable t, Bindable p1, Bindable p2, Bindable i, Nominal m) => (p1 -> p2) -> Nf t p1 m i -> Nf t p2 m i
mapNf3 f (Normal tp tm) = Normal (mapSemantic3 f tp) (mapSemantic3 f tm)

mapNe3 ::(Bindable t, Bindable p1, Bindable p2, Bindable i, Nominal m) => (p1 -> p2) -> Ne t p1 m i -> Ne t p2 m i
mapNe3 f (VarSem a) = VarSem a
mapNe3 f (ApSem ne nf) = (ApSem (mapNe3 f ne) (mapNf3 f nf))
mapNe3 f (FstSem ne) = FstSem (mapNe3 f ne)
mapNe3 f (SndSem ne) = SndSem (mapNe3 f ne)

mapClos3 :: (Bindable t, Bindable p1, Bindable p2, Bindable i, Nominal m) => (p1 -> p2) -> Clos t p1 m i -> Clos t p2 m i
mapClos3 f (Clos ((i, (atom, p)) :. surf) (semanticEnv)) = Clos ((i, (atom, f p)) :. (mapExpr3 f surf)) (fmap (Data.Bifunctor.second (mapSemantic3 f)) semanticEnv)

mapSemantic4 :: (Bindable t1, Bindable t2, Bindable p, Bindable i, Nominal m) => (t1 -> t2) -> Semantic t1 p m i -> Semantic t2 p m i
mapSemantic4 f (LamSem clos)                        = LamSem (mapClos4 f clos)
mapSemantic4 f (NeutralSem typeNeutral termNeutral) = NeutralSem (mapSemantic4 f typeNeutral) (mapNe4 f termNeutral)
mapSemantic4 f (NatTypeSem)                         = NatTypeSem
mapSemantic4 f (NatValueSem i)                      = NatValueSem i
mapSemantic4 f (AddSem s1 s2)                       = AddSem (mapSemantic4 f s1) (mapSemantic4 f s2)
mapSemantic4 f (PiTypeSem s c)                      = PiTypeSem (mapSemantic4 f s) (mapClos4 f c)
mapSemantic4 f (SigTypeSem s c)                     = SigTypeSem (mapSemantic4 f s) (mapClos4 f c)
mapSemantic4 f (PairSem s1 s2)                      = PairSem (mapSemantic4 f s1) (mapSemantic4 f s2)
mapSemantic4 f (UniSem i)                           = UniSem i

mapNf4 :: (Bindable t1, Bindable t2, Bindable p, Bindable i, Nominal m) => (t1 -> t2) -> Nf t1 p m i -> Nf t2 p m i
mapNf4 f (Normal tp tm) = Normal (mapSemantic4 f tp) (mapSemantic4 f tm)

mapNe4 :: (Bindable t1, Bindable t2, Bindable p, Bindable i, Nominal m) => (t1 -> t2) -> Ne t1 p m i -> Ne t2 p m i
mapNe4 f (VarSem a) = VarSem a
mapNe4 f (ApSem ne nf) = (ApSem (mapNe4 f ne) (mapNf4 f nf))
mapNe4 f (FstSem ne) = FstSem (mapNe4 f ne)
mapNe4 f (SndSem ne) = SndSem (mapNe4 f ne)

mapClos4 :: (Bindable t1, Bindable t2, Bindable p, Bindable i, Nominal m) => (t1 -> t2) -> Clos t1 p m i -> Clos t2 p m i
mapClos4 f (Clos ((i, (atom, p)) :. surf) (semanticEnv)) = Clos ((i, (atom, p)) :. (mapExpr4 f surf)) (fmap (\(a, b) -> (a, mapSemantic4 f b)) semanticEnv)

