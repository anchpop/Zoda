module CopyPropagatedProgram where
import ClassyPrelude
import Data.Int
import Data.Ratio
import Nominal hiding ((.))
import Ast
import Control.Applicative (liftA, liftA2, liftA3)
import qualified Data.Bifunctor as Data.Bifunctor
import Data.Functor.Identity

liftA4 f a b c d = liftA3 f a b c <*> d
 
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




traverseExpr :: forall t1 p1 m1 i1 t2 p2 m2 i2 a. (Bindable t1, Bindable t2, Bindable p1, Bindable p2, Bindable i1, Bindable i2, Bindable m1, Bindable m2, Applicative a) => (t1 -> a t2) -> (p1 -> a p2) -> (m1 -> a m2) ->  (i1 -> a i2) -> Expression t1 p1 m1 i1 -> a (Expression t2 p2 m2 i2)
traverseExpr ft fp fm fi = me
  where me :: Expression t1 p1 m1 i1 -> a (Expression t2 p2 m2 i2)
        me (ParenthesizedExpression e t p) = liftA3 ParenthesizedExpression (me e) (ft t) (fp p)
        me (FirstExpression e t p)         = (liftA3 FirstExpression) (me e) (ft t) (fp p)
        me (SecondExpression e t p)        = (liftA3 SecondExpression) (me e) (ft t) (fp p)
        me (PairExpression e1 e2 t p)      = (liftA4 PairExpression) (me e1) (me e2) (ft t) (fp p) -- 
        me (TSigmaBinding e1 ((NoBind i, (a, NoBind p1)) :. e2) t p2) = liftA4 TSigmaBinding (me e1) (liftA2 abst (liftA2 (\x y -> (NoBind x, (a, NoBind y))) (fi i) (fp p1)) (me e2)) (ft t) (fp p2)
        me (UniverseExpression i t p)      = (liftA2 (UniverseExpression i)) (ft t) (fp p)
        me (NumberLiteral i t p)           = (liftA2 (NumberLiteral i)) (ft t) (fp p)
        me (AddExpression e1 e2 t p)       = (liftA4 AddExpression) (me e1) (me e2) (ft t) (fp p)
        me (ReferenceVariable i m t p)     = (liftA4 ReferenceVariable) (fi i) (fm m) (ft t) (fp p)
        me (LambdaVariable (i, a) t p)     = (liftA3 LambdaVariable) ((\x -> (x, a)) <$> fi i) (ft t) (fp p)
        me (FunctionLiteralExpression flit t p) = (liftA3 FunctionLiteralExpression) (traverseFunctionLiteral ft fp fm fi flit) (ft t) (fp p)
        me (FunctionApplicationExpression func args t p) = liftA4 FunctionApplicationExpression (me func) (traverse me args) (ft t) (fp p)
        me (TArrowBinding telescope t p) = (liftA3 TArrowBinding) (traverseTelescope ft fp fm fi telescope) (ft t) (fp p)
        me (Annotation e1 e2 t p) = (liftA4 Annotation) (me e1) (me e2) (ft t) (fp p)
        me (NatTypeExpression t p) = (liftA2 NatTypeExpression) (ft t) (fp p)

traverseTelescope :: forall t1 p1 m1 i1 t2 p2 m2 i2 a. (Bindable t1, Bindable t2, Bindable p1, Bindable p2, Bindable i1, Bindable i2, Bindable m1, Bindable m2, Applicative a) => (t1 -> a t2) -> (p1 -> a p2) -> (m1 -> a m2) ->  (i1 -> a i2) -> Telescope t1 p1 m1 i1 -> a (Telescope t2 p2 m2 i2)
traverseTelescope ft fp fm fi = te
  where te (Scope ((Just (NoBind i, (a, NoBind p)), NoBind e)  :. scope))  = liftA Scope (liftA2 abst (liftA3 (\x y z -> (Just (NoBind x, (a, NoBind y)), NoBind z)) (fi i) (fp p) (traverseExpr ft fp fm fi e)) (te scope))
        te (Scope ((Nothing                       , NoBind e)  :. scope))  = liftA Scope (liftA2 abst (liftA (\x -> (Nothing, NoBind x)) (traverseExpr ft fp fm fi e)) (te scope)) 
        te (Pi    ((Just (NoBind i, (a, NoBind p)), NoBind e)  :. eBound)) = liftA Pi    (liftA2 abst (liftA3 (\x y z -> (Just (NoBind x, (a, NoBind y)), NoBind z)) (fi i) (fp p) (traverseExpr ft fp fm fi e)) (traverseExpr ft fp fm fi eBound))
        te (Pi    ((Nothing                       , NoBind e)  :. eBound)) = liftA Pi    (liftA2 abst (liftA (\x -> (Nothing, NoBind x)) (traverseExpr ft fp fm fi e)) (traverseExpr ft fp fm fi eBound)) 


traverseFunctionLiteral :: forall t1 p1 m1 i1 t2 p2 m2 i2 a. (Bindable t1, Bindable t2, Bindable p1, Bindable p2, Bindable i1, Bindable i2, Bindable m1, Bindable m2, Applicative a) => (t1 -> a t2) -> (p1 -> a p2) -> (m1 -> a m2) ->  (i1 -> a i2) -> FunctionLiteral t1 p1 m1 i1 -> a (FunctionLiteral t2 p2 m2 i2)
traverseFunctionLiteral ft fp fm fi = fe
  where fe :: FunctionLiteral t1 p1 m1 i1 -> a (FunctionLiteral t2 p2 m2 i2)
        fe (Arg     (((NoBind i, (a, NoBind p)), NoBind e) :. nxtArg)) = liftA Arg     (liftA2 abst (liftA3 (\x y z -> ((NoBind x, (a, NoBind y)), NoBind z)) (fi i) (fp p) (traverseExpr ft fp fm fi e)) (fe nxtArg))
        fe (LastArg (((NoBind i, (a, NoBind p)), NoBind e) :. eBound)) = liftA LastArg (liftA2 abst (liftA3 (\x y z -> ((NoBind x, (a, NoBind y)), NoBind z)) (fi i) (fp p) (traverseExpr ft fp fm fi e)) (traverseExpr ft fp fm fi eBound))

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


mapSemantic ft fp fm fi = ms 
  where ms (LamSem clos)         = LamSem $ mapClos ft fp fm fi clos
        ms (NeutralSem tp term)  = NeutralSem (ms tp) (mapNe ft fp fm fi term)
        ms (NatTypeSem)          = NatTypeSem
        ms (NatValueSem i)       = NatValueSem i
        ms (PiTypeSem sem clos)  = PiTypeSem (ms sem) (mapClos ft fp fm fi clos)
        ms (SigTypeSem sem clos) = SigTypeSem (ms sem) (mapClos ft fp fm fi clos)
        ms (PairSem sem1 sem2)   = PairSem (ms sem1) (ms sem2)
        ms (UniSem i)            = UniSem i

mapClos ft fp fm fi (Clos (a :. term) env) = (Clos (a :. mapExpr ft fp fm fi term) (fmap (Data.Bifunctor.second (mapSemantic ft fp fm fi)) env))
mapNe    ft fp fm fi = mn 
  where mn (VarSem a)      = VarSem a
        mn (ApSem ne nf)   = ApSem (mn ne) (mapNf ft fp fm fi nf)
        mn (FstSem ne)     = FstSem (mn ne) 
        mn (SndSem ne)     = SndSem (mn ne) 
        mn (AddSem1 nf ne) = AddSem1 (mapNf ft fp fm fi nf) (mn ne)
        mn (AddSem2 ne nf) = AddSem2 (mn ne) (mapNf ft fp fm fi nf)
        mn (AddSem3 n1 n2) = AddSem3 (mn n1) (mn n2)

mapNf ft fp fm fi (Normal tp tm) = (Normal (mapSemantic ft fp fm fi tp) (mapSemantic ft fp fm fi tm))  

normalizeExprEnv :: (Functor f, Bifunctor p, Bindable b1, Bindable b2, Bindable b3, Bindable m) => f (p a (Expression b1 b2 m b3)) -> f (p a (Expression () () m ()))
normalizeExprEnv s = fmap (Data.Bifunctor.second normalizeExprMetadata) s

