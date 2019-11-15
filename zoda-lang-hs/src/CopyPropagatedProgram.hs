module CopyPropagatedProgram where
import ClassyPrelude
import Data.Int
import Data.Ratio
import Nominal hiding ((.))
import Ast
import qualified Data.Bifunctor as Data.Bifunctor
 
-- |Semantic values contain terms which have evaluated to a constructor which does not need to be 
-- reduced further. So for instance, Lam and Pair may contain computation further inside the term 
-- but at least the outermost constructor is stable and fully evaluated. 
-- Essentially, this contains the introduction forms.
data Semantic t p m i =
    LamSem (Clos t p m i)
  | NeutralSem {tpNeutral   :: Semantic t p m i,  -- ^This should be the type of the neutral term
                termNeutral :: Ne       t p m i}  -- ^This should be the neutral term iteslf
  | NatTypeSem
  | NatValueSem Integer
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
-- Essentially, this contains the elimination forms (since they might get blocked on an argument). 
data Ne t p m i =
    VarSem Atom
  | ApSem (Ne t p m i) (Nf t p m i)
  | FstSem (Ne t p m i)
  | SndSem (Ne t p m i)
  -- For addition, we might be blocked on the first, second, or both arguments 
  -- (this doesn't apply to function application because we can only get blocked if
  -- we don't know what the function is.
  | AddSem1 (Nf t p m i) (Ne t p m i)
  | AddSem2 (Ne t p m i) (Nf t p m i)
  | AddSem3 (Ne t p m i) (Ne t p m i)
  deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal)

-- |nf is a special class of values coming from the style of NbE we use. It associates a type 
-- with a value so that later during quotation we can eta expand it appropriately
data Nf t p m i =
  Normal {tpNf :: Semantic t p m i, termNf :: Semantic t p m i}
  deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal)

type UniLevel = Integer

type SurfaceEnv t p m i = [(Atom, Expression t p m i)]

type SemanticEnv t p m i = [(Atom, Semantic t p m i)]
data Clos t p m i = Clos {termClos :: (Bind Atom (Expression t p m i)), envClos :: (SemanticEnv t p m i)}
  deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal) 


















mapExpr1 :: (Bindable t, Bindable p, Bindable i1, Bindable i2, Bindable m ) => (i1 -> i2) -> Expression t p m i1 -> Expression t p m i2
mapExpr1 f (ParenthesizedExpression e t p)               = ParenthesizedExpression (mapExpr1 f e) t p
mapExpr1 f (FirstExpression e t p)                       = FirstExpression (mapExpr1 f e) t p
mapExpr1 f (SecondExpression e t p)                      = SecondExpression (mapExpr1 f e) t p
mapExpr1 f (PairExpression e1 e2 t p)                    = PairExpression (mapExpr1 f e1) (mapExpr1 f e2) t p
mapExpr1 f (TSigmaBinding e1 (a :. e2) t p)              = TSigmaBinding (mapExpr1 f e1) ((Data.Bifunctor.first (fmap f) a) :. (mapExpr1 f e2)) t p
mapExpr1 _ (UniverseExpression i t p)                    = UniverseExpression i t p
mapExpr1 _ (NumberLiteral i1 t p)                        = NumberLiteral i1 t p
mapExpr1 f (AddExpression e1 e2 t p)                     = AddExpression (mapExpr1 f e1) (mapExpr1 f e2) t p
mapExpr1 f (ReferenceVariable i m t p)                   = ReferenceVariable (f i) m t p 
mapExpr1 f (LambdaVariable (i, a) t p)                   = LambdaVariable ((f i), a) t p
mapExpr1 f (FunctionLiteralExpression (a :. e) t p)      = FunctionLiteralExpression ((fmap (Data.Bifunctor.first (fmap f)) a) :. (mapExpr1 f e)) t p
mapExpr1 f (FunctionApplicationExpression func args t p) = FunctionApplicationExpression (mapExpr1 f func) (fmap (mapExpr1 f) args) t p
mapExpr1 f (TArrowBinding telescope t p)                 = TArrowBinding (mapTelescope1 f telescope) t p
mapExpr1 f (Annotation e1 e2 t p)                        = Annotation (mapExpr1 f e1) (mapExpr1 f e2) t p
mapExpr1 _ (NatTypeExpression t p)                       = NatTypeExpression t p


mapExpr2 :: (Bindable t, Bindable p, Bindable i, Bindable m1, Bindable m2) => (m1 -> m2) -> Expression t p m1 i -> Expression t p m2 i
mapExpr2 f (ParenthesizedExpression e t p)               = ParenthesizedExpression (mapExpr2 f e) t p
mapExpr2 f (FirstExpression e t p)                       = FirstExpression (mapExpr2 f e) t p
mapExpr2 f (SecondExpression e t p)                      = SecondExpression (mapExpr2 f e) t p
mapExpr2 f (PairExpression e1 e2 t p)                    = PairExpression (mapExpr2 f e1) (mapExpr2 f e2) t p
mapExpr2 f (TSigmaBinding e1 (a :. e2) t p)              = TSigmaBinding (mapExpr2 f e1) (a :. (mapExpr2 f e2)) t p
mapExpr2 _ (UniverseExpression i t p)                    = UniverseExpression i t p
mapExpr2 _ (NumberLiteral i1 t p)                        = NumberLiteral i1 t p
mapExpr2 f (AddExpression e1 e2 t p)                     = AddExpression (mapExpr2 f e1) (mapExpr2 f e2) t p
mapExpr2 f (ReferenceVariable i m t p)                   = ReferenceVariable i (f m) t p 
mapExpr2 _ (LambdaVariable i t p)                        = LambdaVariable i t p
mapExpr2 f (FunctionLiteralExpression (a :. e) t p)      = FunctionLiteralExpression (a :. (mapExpr2 f e)) t p
mapExpr2 f (FunctionApplicationExpression func args t p) = FunctionApplicationExpression (mapExpr2 f func) (fmap (mapExpr2 f) args) t p
mapExpr2 f (TArrowBinding telescope t p)                 = TArrowBinding (mapTelescope2 f telescope) t p
mapExpr2 f (Annotation e1 e2 t p)                        = Annotation (mapExpr2 f e1) (mapExpr2 f e2) t p
mapExpr2 _ (NatTypeExpression t p)                       = NatTypeExpression t p



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
traverseExpr2 _ (UniverseExpression i t p)                    = pure $ UniverseExpression i t p
traverseExpr2 _ (NumberLiteral i1 t p)                     = pure $ NumberLiteral i1 t p
traverseExpr2 f (AddExpression e1 e2 t p)                     = do
  e1' <- traverseExpr2 f e1
  e2' <- traverseExpr2 f e2
  pure $ AddExpression e1' e2' t p
traverseExpr2 f (ReferenceVariable i m t p)                   = do
  m' <- f m 
  pure $ ReferenceVariable i m' t p 
traverseExpr2 _ (LambdaVariable i t p)                        = pure $ LambdaVariable i t p
traverseExpr2 f (FunctionLiteralExpression (a :. e) t p)      = do
  e' <- traverseExpr2 f e
  pure $ FunctionLiteralExpression (a :. e') t p
traverseExpr2 f (FunctionApplicationExpression func args t p) = do
  func' <- traverseExpr2 f func
  args' <- traverse (traverseExpr2 f) args 
  pure $ FunctionApplicationExpression func' args' t p
traverseExpr2 f (TArrowBinding telescope t p)              = do 
  telescope' <- traverseTelescope2 f telescope
  pure $ TArrowBinding telescope' t p
traverseExpr2 f (Annotation e1 e2 t p)                        = do 
  e1' <- traverseExpr2 f e1 
  e2' <- traverseExpr2 f e2
  pure $ Annotation e1' e2' t p
traverseExpr2 _ (NatTypeExpression t p)                       = pure $ NatTypeExpression t p

forExpr2 :: (Bindable t, Bindable p, Bindable i, Monad mo, Nominal m1, Nominal m2) => Expression t p m1 i -> (m1 -> mo m2) -> mo (Expression t p m2 i)
forExpr2 x y = traverseExpr2 y x

traverseTelescope2 f (Scope ((a, NoBind expr) :. scope)) = do 
  expr'  <- traverseExpr2 f expr
  scope' <- traverseTelescope2 f scope
  pure $ Scope ((a, NoBind expr') :. scope')
traverseTelescope2 f (Pi    ((a, NoBind expr) :. expr2)) = do
  expr'  <- traverseExpr2 f expr
  expr2' <- traverseExpr2 f expr2
  pure $ Pi    ((a, NoBind expr') :. expr2')




copyIdentifierOntoMetadata :: (Bindable t, Bindable p, Bindable i, Bindable m) => Expression t p m i -> Expression t p (i, p) i
copyIdentifierOntoMetadata (ParenthesizedExpression e t p)               = ParenthesizedExpression (copyIdentifierOntoMetadata e) t p
copyIdentifierOntoMetadata (FirstExpression e t p)                       = FirstExpression (copyIdentifierOntoMetadata e) t p
copyIdentifierOntoMetadata (SecondExpression e t p)                      = SecondExpression (copyIdentifierOntoMetadata e) t p
copyIdentifierOntoMetadata (PairExpression e1 e2 t p)                    = PairExpression (copyIdentifierOntoMetadata e1) (copyIdentifierOntoMetadata e2) t p
copyIdentifierOntoMetadata (TSigmaBinding e1 (a :. e2) t p)              = TSigmaBinding (copyIdentifierOntoMetadata e1) (a :. (copyIdentifierOntoMetadata e2)) t p
copyIdentifierOntoMetadata (UniverseExpression i t p)                    = UniverseExpression i t p
copyIdentifierOntoMetadata (NumberLiteral i1 t p)                        = NumberLiteral i1 t p
copyIdentifierOntoMetadata (AddExpression e1 e2 t p)                     = AddExpression (copyIdentifierOntoMetadata e1) (copyIdentifierOntoMetadata e2) t p
copyIdentifierOntoMetadata (ReferenceVariable i _ t p)                   = ReferenceVariable i (i, p) t p 
copyIdentifierOntoMetadata (LambdaVariable i t p)                        = LambdaVariable i t p
copyIdentifierOntoMetadata (FunctionLiteralExpression (a :. e) t p)      = FunctionLiteralExpression (a :. (copyIdentifierOntoMetadata e)) t p
copyIdentifierOntoMetadata (FunctionApplicationExpression func args t p) = FunctionApplicationExpression (copyIdentifierOntoMetadata func) (fmap (copyIdentifierOntoMetadata) args) t p
copyIdentifierOntoMetadata (TArrowBinding telescope t p)                 = TArrowBinding (copyIdentifierOntoTelescopeMetadata telescope) t p
copyIdentifierOntoMetadata (Annotation e1 e2 t p)                        = Annotation (copyIdentifierOntoMetadata e1) (copyIdentifierOntoMetadata e2) t p
copyIdentifierOntoMetadata (NatTypeExpression t p)                       = NatTypeExpression t p

copyIdentifierOntoTelescopeMetadata :: (Bindable t, Bindable i, Bindable p, Bindable m) => Telescope t p m i -> Telescope t p (i, p) i
copyIdentifierOntoTelescopeMetadata (Scope ((b, NoBind expr) :. scope)) = Scope ((b, NoBind (copyIdentifierOntoMetadata expr)) :. (copyIdentifierOntoTelescopeMetadata scope))
copyIdentifierOntoTelescopeMetadata (Pi    ((b, NoBind expr) :. expr2)) = Pi    ((b, NoBind (copyIdentifierOntoMetadata expr)) :. (copyIdentifierOntoMetadata expr2))


mapExpr3 :: (Bindable t, Bindable p1, Bindable p2, Bindable i, Bindable m) => (p1 -> p2) -> Expression t p1 m i -> Expression t p2 m i
mapExpr3 f (ParenthesizedExpression e t p)               = ParenthesizedExpression (mapExpr3 f e) t (f p)
mapExpr3 f (FirstExpression e t p)                       = FirstExpression (mapExpr3 f e) t (f p)
mapExpr3 f (SecondExpression e t p)                      = SecondExpression (mapExpr3 f e) t (f p)
mapExpr3 f (PairExpression e1 e2 t p)                    = PairExpression (mapExpr3 f e1) (mapExpr3 f e2) t (f p)
mapExpr3 f (TSigmaBinding e1 (a :. e2) t p)              = TSigmaBinding (mapExpr3 f e1) ((Data.Bifunctor.second (Data.Bifunctor.second (fmap f)) a) :. (mapExpr3 f e2)) t (f p)
mapExpr3 f (UniverseExpression i t p)                    = UniverseExpression i t (f p)
mapExpr3 f (NumberLiteral i1 t p)                        = NumberLiteral i1 t (f p)
mapExpr3 f (AddExpression e1 e2 t p)                     = AddExpression (mapExpr3 f e1) (mapExpr3 f e2) t (f p)
mapExpr3 f (ReferenceVariable i m t p)                   = ReferenceVariable i m t (f p) 
mapExpr3 f (LambdaVariable i t p)                        = LambdaVariable i t (f p)
mapExpr3 f (FunctionLiteralExpression (a :. e) t p)      = FunctionLiteralExpression ((fmap (Data.Bifunctor.second (Data.Bifunctor.second (fmap f))) a) :. (mapExpr3 f e)) t (f p)
mapExpr3 f (FunctionApplicationExpression func args t p) = FunctionApplicationExpression (mapExpr3 f func) (fmap (mapExpr3 f) args) t (f p)
mapExpr3 f (TArrowBinding telescope t p)                 = TArrowBinding (mapTelescope3 f telescope) t (f p)
mapExpr3 f (Annotation e1 e2 t p)                        = Annotation (mapExpr3 f e1) (mapExpr3 f e2) t (f p)
mapExpr3 f (NatTypeExpression t p)                       = NatTypeExpression t (f p)


mapExpr4 :: (Bindable p, Bindable i, Bindable t1, Bindable t2, Bindable m) => (t1 -> t2) -> Expression t1 p m i -> Expression t2 p m i
mapExpr4 f (ParenthesizedExpression e t p)               = ParenthesizedExpression (mapExpr4 f e) (f t) p
mapExpr4 f (FirstExpression e t p)                       = FirstExpression (mapExpr4 f e) (f t) p
mapExpr4 f (SecondExpression e t p)                      = SecondExpression (mapExpr4 f e) (f t) p
mapExpr4 f (PairExpression e1 e2 t p)                    = PairExpression (mapExpr4 f e1) (mapExpr4 f e2) (f t) p
mapExpr4 f (TSigmaBinding e1 (a :. e2) t p)              = TSigmaBinding (mapExpr4 f e1) (a :. (mapExpr4 f e2)) (f t) p
mapExpr4 f (UniverseExpression i t p)                    = UniverseExpression i (f t) p
mapExpr4 f (NumberLiteral i1 t p)                        = NumberLiteral i1 (f t) p
mapExpr4 f (AddExpression e1 e2 t p)                     = AddExpression (mapExpr4 f e1) (mapExpr4 f e2) (f t) p
mapExpr4 f (ReferenceVariable i m t p)                   = ReferenceVariable i m (f t) p 
mapExpr4 f (LambdaVariable i t p)                        = LambdaVariable i (f t) p
mapExpr4 f (FunctionLiteralExpression (a :. e) t p)      = FunctionLiteralExpression (a :. (mapExpr4 f e)) (f t) p
mapExpr4 f (FunctionApplicationExpression func args t p) = FunctionApplicationExpression (mapExpr4 f func) (fmap (mapExpr4 f) args) (f t) p
mapExpr4 f (TArrowBinding telescope t p)                 = TArrowBinding (mapTelescope4 f telescope) (f t) p
mapExpr4 f (Annotation e1 e2 t p)                        = Annotation (mapExpr4 f e1) (mapExpr4 f e2) (f t) p
mapExpr4 f (NatTypeExpression t p)                       = NatTypeExpression (f t) p


mapSemantic1 :: (Bindable t, Bindable i1, Bindable i2, Bindable p, Bindable m) => (i1 -> i2) -> Semantic t p m i1 -> Semantic t p m i2
mapSemantic1 f (LamSem clos)                        = LamSem (mapClos1 f clos)
mapSemantic1 f (NeutralSem typeNeutral termNeutral) = NeutralSem (mapSemantic1 f typeNeutral) (mapNe1 f termNeutral)
mapSemantic1 _ (NatTypeSem)                         = NatTypeSem
mapSemantic1 _ (NatValueSem i)                      = NatValueSem i
mapSemantic1 f (PiTypeSem s c)                      = PiTypeSem (mapSemantic1 f s) (mapClos1 f c)
mapSemantic1 f (SigTypeSem s c)                     = SigTypeSem (mapSemantic1 f s) (mapClos1 f c)
mapSemantic1 f (PairSem s1 s2)                      = PairSem (mapSemantic1 f s1) (mapSemantic1 f s2)
mapSemantic1 _ (UniSem i)                           = UniSem i
 
mapTelescope1 :: (Bindable t, Bindable i1, Bindable i2, Bindable p, Bindable m) => (i1 -> i2) -> Telescope t p m i1 -> Telescope t p m i2
mapTelescope1 f (Scope ((Just (NoBind i, (a, NoBind p)), NoBind expr) :. scope)) = Scope ((Just (NoBind (f i), (a, NoBind p)), NoBind (mapExpr1 f expr)) :. (mapTelescope1 f scope))
mapTelescope1 f (Scope ((Nothing                       , NoBind expr) :. scope)) = Scope ((Nothing,                            NoBind (mapExpr1 f expr)) :. (mapTelescope1 f scope))
mapTelescope1 f (Pi    ((Just (NoBind i, (a, NoBind p)), NoBind expr) :. expr2)) = Pi    ((Just (NoBind (f i), (a, NoBind p)), NoBind (mapExpr1 f expr)) :. (mapExpr1 f expr2))
mapTelescope1 f (Pi    ((Nothing                       , NoBind expr) :. expr2)) = Pi    ((Nothing,                            NoBind (mapExpr1 f expr)) :. (mapExpr1 f expr2))

mapNf1 :: (Bindable t, Bindable i1, Bindable i2, Bindable p, Bindable m) => (i1 -> i2) -> Nf t p m i1 -> Nf t p m i2
mapNf1 f (Normal tp tm) = Normal (mapSemantic1 f tp) (mapSemantic1 f tm)

mapNe1 :: (Bindable t, Bindable i1, Bindable i2, Bindable p, Bindable m) => (i1 -> i2) -> Ne t p m i1 -> Ne t p m i2
mapNe1 _ (VarSem a) = VarSem a
mapNe1 f (ApSem ne nf) = ApSem (mapNe1 f ne) (mapNf1 f nf)
mapNe1 f (FstSem ne) = FstSem (mapNe1 f ne)
mapNe1 f (SndSem ne) = SndSem (mapNe1 f ne)
mapNe1 f (AddSem1 nf ne) = AddSem1 (mapNf1 f nf) (mapNe1 f ne)
mapNe1 f (AddSem2 ne nf) = AddSem2 (mapNe1 f ne) (mapNf1 f nf)
mapNe1 f (AddSem3 ne1 ne2) = AddSem3 (mapNe1 f ne1) (mapNe1 f ne2)

mapClos1 :: (Bindable t, Bindable i1, Bindable i2, Bindable p, Bindable m) => (i1 -> i2) -> Clos t p m i1 -> Clos t p m i2
mapClos1 f (Clos (atom :. surf) semanticEnv) = Clos (atom :. (mapExpr1 f surf)) (fmap (Data.Bifunctor.second (mapSemantic1 f)) semanticEnv)



mapSemantic2 :: (Bindable t, Bindable p, Bindable i, Bindable m1, Bindable m2) => (m1 -> m2) -> Semantic t p m1 i -> Semantic t p m2 i
mapSemantic2 f (LamSem clos)                        = LamSem (mapClos2 f clos)
mapSemantic2 f (NeutralSem typeNeutral termNeutral) = NeutralSem (mapSemantic2 f typeNeutral) (mapNe2 f termNeutral)
mapSemantic2 _ (NatTypeSem)                         = NatTypeSem
mapSemantic2 _ (NatValueSem i)                      = NatValueSem i
mapSemantic2 f (PiTypeSem s c)                      = PiTypeSem (mapSemantic2 f s) (mapClos2 f c)
mapSemantic2 f (SigTypeSem s c)                     = SigTypeSem (mapSemantic2 f s) (mapClos2 f c)
mapSemantic2 f (PairSem s1 s2)                      = PairSem (mapSemantic2 f s1) (mapSemantic2 f s2)
mapSemantic2 _ (UniSem i)                           = UniSem i

mapTelescope2 :: (Bindable t, Bindable i, Bindable p, Bindable m1, Bindable m2) => (m1 -> m2) -> Telescope t p m1 i -> Telescope t p m2 i
mapTelescope2 f (Scope ((a, NoBind expr) :. scope)) = Scope ((a, NoBind (mapExpr2 f expr)) :. (mapTelescope2 f scope))
mapTelescope2 f (Pi    ((a, NoBind expr) :. expr2)) = Pi    ((a, NoBind (mapExpr2 f expr)) :. (mapExpr2 f expr2))


mapNf2 :: (Bindable t, Bindable p, Bindable i, Bindable m1, Bindable m2) => (m1 -> m2) -> Nf t p m1 i -> Nf t p m2 i
mapNf2 f (Normal tp tm) = Normal (mapSemantic2 f tp) (mapSemantic2 f tm)

mapNe2 :: (Bindable t, Bindable p, Bindable i, Bindable m1, Bindable m2) => (m1 -> m2) -> Ne t p m1 i -> Ne t p m2 i
mapNe2 _ (VarSem a) = VarSem a
mapNe2 f (ApSem ne nf) = (ApSem (mapNe2 f ne) (mapNf2 f nf))
mapNe2 f (FstSem ne) = FstSem (mapNe2 f ne)
mapNe2 f (SndSem ne) = SndSem (mapNe2 f ne)
mapNe2 f (AddSem1 nf ne) = AddSem1 (mapNf2 f nf) (mapNe2 f ne)
mapNe2 f (AddSem2 ne nf) = AddSem2 (mapNe2 f ne) (mapNf2 f nf)
mapNe2 f (AddSem3 ne1 ne2) = AddSem3 (mapNe2 f ne1) (mapNe2 f ne2)

mapClos2 :: (Bindable t, Bindable p, Bindable i, Bindable m1, Bindable m2) => (m1 -> m2) -> Clos t p m1 i -> Clos t p m2 i
mapClos2 f (Clos (atom :. surf) semanticEnv) = Clos (atom :. (mapExpr2 f surf)) (fmap (\(a, b) -> (a, mapSemantic2 f b)) semanticEnv)


mapSemantic3 ::(Bindable t, Bindable p1, Bindable p2, Bindable i, Bindable m) => (p1 -> p2) -> Semantic t p1 m i -> Semantic t p2 m i
mapSemantic3 f (LamSem clos)                        = LamSem (mapClos3 f clos)
mapSemantic3 f (NeutralSem typeNeutral termNeutral) = NeutralSem (mapSemantic3 f typeNeutral) (mapNe3 f termNeutral)
mapSemantic3 _ (NatTypeSem)                         = NatTypeSem
mapSemantic3 _ (NatValueSem i)                      = NatValueSem i
mapSemantic3 f (PiTypeSem s c)                      = PiTypeSem (mapSemantic3 f s) (mapClos3 f c)
mapSemantic3 f (SigTypeSem s c)                     = SigTypeSem (mapSemantic3 f s) (mapClos3 f c)
mapSemantic3 f (PairSem s1 s2)                      = PairSem (mapSemantic3 f s1) (mapSemantic3 f s2)
mapSemantic3 _ (UniSem i)                           = UniSem i

mapTelescope3 :: (Bindable t, Bindable i, Bindable p1, Bindable p2, Bindable m) => (p1 -> p2) -> Telescope t p1 m i -> Telescope t p2 m i
mapTelescope3 f (Scope ((Just (NoBind i, (a, NoBind p)), NoBind expr) :. scope)) = Scope ((Just (NoBind i, (a, NoBind (f p))), NoBind (mapExpr3 f expr)) :. (mapTelescope3 f scope))
mapTelescope3 f (Scope ((Nothing                       , NoBind expr) :. scope)) = Scope ((Nothing,                            NoBind (mapExpr3 f expr)) :. (mapTelescope3 f scope))
mapTelescope3 f (Pi    ((Just (NoBind i, (a, NoBind p)), NoBind expr) :. expr2)) = Pi    ((Just (NoBind i, (a, NoBind (f p))), NoBind (mapExpr3 f expr)) :. (mapExpr3 f expr2))
mapTelescope3 f (Pi    ((Nothing                       , NoBind expr) :. expr2)) = Pi    ((Nothing,                            NoBind (mapExpr3 f expr)) :. (mapExpr3 f expr2))

mapNf3 ::(Bindable t, Bindable p1, Bindable p2, Bindable i, Bindable m) => (p1 -> p2) -> Nf t p1 m i -> Nf t p2 m i
mapNf3 f (Normal tp tm) = Normal (mapSemantic3 f tp) (mapSemantic3 f tm)

mapNe3 ::(Bindable t, Bindable p1, Bindable p2, Bindable i, Bindable m) => (p1 -> p2) -> Ne t p1 m i -> Ne t p2 m i
mapNe3 _ (VarSem a) = VarSem a
mapNe3 f (ApSem ne nf) = (ApSem (mapNe3 f ne) (mapNf3 f nf))
mapNe3 f (FstSem ne) = FstSem (mapNe3 f ne)
mapNe3 f (SndSem ne) = SndSem (mapNe3 f ne)
mapNe3 f (AddSem1 nf ne) = AddSem1 (mapNf3 f nf) (mapNe3 f ne)
mapNe3 f (AddSem2 ne nf) = AddSem2 (mapNe3 f ne) (mapNf3 f nf)
mapNe3 f (AddSem3 ne1 ne2) = AddSem3 (mapNe3 f ne1) (mapNe3 f ne2)

mapClos3 :: (Bindable t, Bindable p1, Bindable p2, Bindable i, Bindable m) => (p1 -> p2) -> Clos t p1 m i -> Clos t p2 m i
mapClos3 f (Clos (atom :. surf) semanticEnv) = Clos (atom :. (mapExpr3 f surf)) (fmap (Data.Bifunctor.second (mapSemantic3 f)) semanticEnv)

mapSemantic4 :: (Bindable t1, Bindable t2, Bindable p, Bindable i, Bindable m) => (t1 -> t2) -> Semantic t1 p m i -> Semantic t2 p m i
mapSemantic4 f (LamSem clos)                        = LamSem (mapClos4 f clos)
mapSemantic4 f (NeutralSem typeNeutral termNeutral) = NeutralSem (mapSemantic4 f typeNeutral) (mapNe4 f termNeutral)
mapSemantic4 _ (NatTypeSem)                         = NatTypeSem
mapSemantic4 _ (NatValueSem i)                      = NatValueSem i
mapSemantic4 f (PiTypeSem s c)                      = PiTypeSem (mapSemantic4 f s) (mapClos4 f c)
mapSemantic4 f (SigTypeSem s c)                     = SigTypeSem (mapSemantic4 f s) (mapClos4 f c)
mapSemantic4 f (PairSem s1 s2)                      = PairSem (mapSemantic4 f s1) (mapSemantic4 f s2)
mapSemantic4 _ (UniSem i)                           = UniSem i

mapTelescope4 :: (Bindable t1, Bindable t2, Bindable i, Bindable p, Bindable m) => (t1 -> t2) -> Telescope t1 p m i -> Telescope t2 p m i
mapTelescope4 f (Scope ((Just (NoBind i, (a, NoBind p)), NoBind expr) :. scope)) = Scope ((Just (NoBind i, (a, NoBind p)), NoBind (mapExpr4 f expr)) :. (mapTelescope4 f scope))
mapTelescope4 f (Scope ((Nothing                       , NoBind expr) :. scope)) = Scope ((Nothing,                        NoBind (mapExpr4 f expr)) :. (mapTelescope4 f scope))
mapTelescope4 f (Pi    ((Just (NoBind i, (a, NoBind p)), NoBind expr) :. expr2)) = Pi    ((Just (NoBind i, (a, NoBind p)), NoBind (mapExpr4 f expr)) :. (mapExpr4 f expr2))
mapTelescope4 f (Pi    ((Nothing                       , NoBind expr) :. expr2)) = Pi    ((Nothing,                        NoBind (mapExpr4 f expr)) :. (mapExpr4 f expr2))

mapNf4 :: (Bindable t1, Bindable t2, Bindable p, Bindable i, Bindable m) => (t1 -> t2) -> Nf t1 p m i -> Nf t2 p m i
mapNf4 f (Normal tp tm) = Normal (mapSemantic4 f tp) (mapSemantic4 f tm)

mapNe4 :: (Bindable t1, Bindable t2, Bindable p, Bindable i, Bindable m) => (t1 -> t2) -> Ne t1 p m i -> Ne t2 p m i
mapNe4 _ (VarSem a) = VarSem a
mapNe4 f (ApSem ne nf) = (ApSem (mapNe4 f ne) (mapNf4 f nf))
mapNe4 f (FstSem ne) = FstSem (mapNe4 f ne)
mapNe4 f (SndSem ne) = SndSem (mapNe4 f ne)
mapNe4 f (AddSem1 nf ne) = AddSem1 (mapNf4 f nf) (mapNe4 f ne)
mapNe4 f (AddSem2 ne nf) = AddSem2 (mapNe4 f ne) (mapNf4 f nf)
mapNe4 f (AddSem3 ne1 ne2) = AddSem3 (mapNe4 f ne1) (mapNe4 f ne2)

mapClos4 :: (Bindable t1, Bindable t2, Bindable p, Bindable i, Bindable m) => (t1 -> t2) -> Clos t1 p m i -> Clos t2 p m i
mapClos4 f (Clos (atom :. surf) semanticEnv) = Clos (atom :. (mapExpr4 f surf)) (fmap (\(a, b) -> (a, mapSemantic4 f b)) semanticEnv)

normalizeNeMetadata :: (Bindable b1, Bindable b2, Bindable b3, Bindable m) => Ne b1 b2 m b3 -> Ne () () m ()
normalizeNeMetadata e = mapNe4 (const ()) $ mapNe3 (const ()) $ mapNe1 (const ()) $ e
normalizeNfMetadata :: (Bindable b1, Bindable b2, Bindable b3, Bindable m) => Nf b1 b2 m b3 -> Nf () () m ()
normalizeNfMetadata e = mapNf4 (const ()) $ mapNf3 (const ()) $ mapNf1 (const ()) $ e
normalizeExprMetadata :: (Bindable b1, Bindable b2, Bindable b3, Bindable m) => Expression b1 b2 m b3 -> Expression () () m ()
normalizeExprMetadata e = mapExpr4 (const ()) $ mapExpr3 (const ()) $ mapExpr1 (const ()) $ e
normalizeSemanticMetadata :: (Bindable b1, Bindable b2, Bindable b3, Bindable m) => Semantic b1 b2 m b3 -> Semantic () () m ()
normalizeSemanticMetadata s = mapSemantic4 (const ()) $ mapSemantic3 (const ()) $ mapSemantic1 (const ()) $ s
normalizeClosureMetadata :: (Bindable b1, Bindable b2, Bindable b3, Bindable m) => Clos b1 b2 m b3 -> Clos () () m ()
normalizeClosureMetadata  s = mapClos4 (const ()) $ mapClos3 (const ()) $ mapClos1 (const ()) $ s
normalizeSemanticEnv :: (Functor f, Bifunctor p, Bindable b1, Bindable b2, Bindable b3, Bindable m) => f (p a (Semantic b1 b2 m b3)) -> f (p a (Semantic () () m ()))
normalizeSemanticEnv s = fmap (Data.Bifunctor.second normalizeSemanticMetadata) s
normalizeExprEnv :: (Functor f, Bifunctor p, Bindable b1, Bindable b2, Bindable b3, Bindable m) => f (p a (Expression b1 b2 m b3)) -> f (p a (Expression () () m ()))
normalizeExprEnv s = fmap (Data.Bifunctor.second normalizeExprMetadata) s
normalizeClosureArg :: (a1, (a2, b)) -> ((), (a2, ()))
normalizeClosureArg (_, (a, _)) = ((), (a, ()))