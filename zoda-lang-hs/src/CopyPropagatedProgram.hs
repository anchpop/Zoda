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
data Semantic i m n =
    LamSem (Clos i m n)
  | NeutralSem {tpNeutral   :: Semantic i m n,  -- ^This should be the type of the neutral term
                termNeutral :: Ne       i m n}  -- ^This should be the neutral term iteslf
  | NatTypeSem
  | NatValueSem Integer
  | PiTypeSem (Semantic i m n) (Clos i m n)
  | SigTypeSem (Semantic i m n) (Clos i m n) 
  | PairSem (Semantic i m n) (Semantic i m n)
  | TypeConstructorSem n m
  | DataConstructorSem Int
  | UniSem UniLevel
  deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal)

-- |We also have to consider the case that something wants to reduce further before becoming a 
-- value but cannot because its blocked on something. These are called neutral terms. The 
-- canonical example of a neutral term is just a variable x. It's not yet a value but there's 
-- no way to convert it to a value since we have no information on what x is yet. Similarly, if 
-- we have some neutral term and we apply Fst to it it's clearly still not a value but we don't 
-- have any way of reducing it further so what's there to do. 
-- Essentially, this contains the elimination forms (since they might get blocked on an argument). 
data Ne i m n =
    VarSem Atom
  | ApSem (Ne i m n) (Nf i m n)
  | FstSem (Ne i m n)
  | SndSem (Ne i m n)
  -- For addition, we might be blocked on the first, second, or both arguments 
  -- (this doesn't apply to function application because we can only get blocked if
  -- we don't know what the function is.
  | AddSem1 (Nf i m n) (Ne i m n)
  | AddSem2 (Ne i m n) (Nf i m n)
  | AddSem3 (Ne i m n) (Ne i m n)
  deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal)

-- |nf is a special class of values coming from the style of NbE we use. It associates a type 
-- with a value so that later during quotation we can eta expand it appropriately
data Nf i m n =
  Normal {tpNf :: Semantic i m n, termNf :: Semantic i m n}
  deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal)

type UniLevel = Integer

type SurfaceEnv i m = [(Atom, ExpressionX Plain i m)]

type SemanticEnv i m n = [(Atom, Semantic i m n)]
data Clos i m t = Clos {termClos :: (Bind Atom (ExpressionX Plain i m)), envClos :: (SemanticEnv i m t)}
  deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal) 

normalizeExprEnv :: (ConstraintX Bindable Plain i m, ConstraintX Bindable Plain () m, Bindable i, Bindable m) => SurfaceEnv i m -> SurfaceEnv () m 
normalizeExprEnv s = fmap (Data.Bifunctor.second plainToNormalizedPlainExpr) s

