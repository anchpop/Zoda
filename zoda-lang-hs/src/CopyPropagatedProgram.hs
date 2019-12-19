module CopyPropagatedProgram where
import ClassyPrelude
import Data.Int
import Data.Ratio
import Nominal hiding ((.))
import Ast
import Control.Applicative (liftA, liftA2, liftA3)
import qualified Data.Bifunctor as Data.Bifunctor
import Data.Functor.Identity

 
-- |Semantic values contain terms which have evaluated to a constructor which does not need to be 
-- reduced further. So for instance, Lam and Pair may contain computation further inside the term 
-- but at least the outermost constructor is stable and fully evaluated. 
-- Essentially, this contains the introduction forms.
data Semantic t p m i i' =
    LamSem (Clos t p m i i')
  | NeutralSem {tpNeutral   :: Semantic t p m i i',  -- ^This should be the type of the neutral term
                termNeutral :: Ne       t p m i i'}  -- ^This should be the neutral term iteslf
  | NatTypeSem
  | NatValueSem Integer
  | PiTypeSem (Semantic t p m i i') (Clos t p m i i')
  | SigTypeSem (Semantic t p m i i') (Clos t p m i i') 
  | PairSem (Semantic t p m i i') (Semantic t p m i i')
  | UniSem UniLevel
  deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal)

-- |We also have to consider the case that something wants to reduce further before becoming a 
-- value but cannot because its blocked on something. These are called neutral terms. The 
-- canonical example of a neutral term is just a variable x. It's not yet a value but there's 
-- no way to convert it to a value since we have no information on what x is yet. Similarly, if 
-- we have some neutral term and we apply Fst to it it's clearly still not a value but we don't 
-- have any way of reducing it further so what's there to do. 
-- Essentially, this contains the elimination forms (since they might get blocked on an argument). 
data Ne t p m i i' =
    VarSem Atom
  | ApSem (Ne t p m i i') (Nf t p m i i')
  | FstSem (Ne t p m i i')
  | SndSem (Ne t p m i i')
  -- For addition, we might be blocked on the first, second, or both arguments 
  -- (this doesn't apply to function application because we can only get blocked if
  -- we don't know what the function is.
  | AddSem1 (Nf t p m i i') (Ne t p m i i')
  | AddSem2 (Ne t p m i i') (Nf t p m i i')
  | AddSem3 (Ne t p m i i') (Ne t p m i i')
  deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal)

-- |nf is a special class of values coming from the style of NbE we use. It associates a type 
-- with a value so that later during quotation we can eta expand it appropriately
data Nf t p m i i' =
  Normal {tpNf :: Semantic t p m i i', termNf :: Semantic t p m i i'}
  deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal)

type UniLevel = Integer

type SurfaceEnv t p m i = [(Atom, Expression t p m i)]

type SemanticEnv t p m i i' = [(Atom, Semantic t p m i i')]
data Clos t p m i i' = Clos {termClos :: (Bind Atom (Expression t p m i)), envClos :: (SemanticEnv t p m i i')}
  deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal) 




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

