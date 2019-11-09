module CopyPropagatedProgramConverter where
import ClassyPrelude hiding (lookup)
import Data.List (lookup)
import Basic
import Data.Maybe
import Ast
import qualified CopyPropagatedProgram as CPP
import qualified Data.Set as Set
import qualified Data.Map.Justified as Map
import qualified Data.Map.Lazy as UMap
import Nominal hiding ((.))

data Metavariable = TypechecksOkay{-MetavariableApplication Int [Metavariable] 
                  | Metavariable Int 
                  | Constant Int-}

data Types i = Number | Bool | Arr (Maybe i) (Types i) (Types i) deriving (Eq, Show, Read)

type UniLevel = Integer
type Surface t p ph i = JustifiedExpression t p ph i 

type SurfaceEnv t p ph i = [(Atom, Surface t p ph i)]


type SemanticEnv t p ph i = [(Atom, Semantic t p ph i)]
data Clos t p ph i = Clos {termClos :: (Bind (i, (Atom, p)) (Surface t p ph i)), envClos :: (SemanticEnv t p ph i)}
  deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal)

-- |Semantic values contain terms which have evaluated to a constructor which does not need to be 
-- reduced further. So for instance, Lam and Pair may contain computation further inside the term 
-- but at least the outermost constructor is stable and fully evaluated. 
data Semantic t p ph i =
    LamSem (Clos t p ph i)
  | NeutralSem {tpNeutral   :: Semantic t p ph i,  -- ^This should be the type of the neutral term
                termNeutral :: Ne       t p ph i}  -- ^This should be the neutral term iteslf
  | NatTypeSem
  | NatValueSem Integer
  | AddSem (Semantic t p ph i) (Semantic t p ph i)
  | PiTypeSem (Semantic t p ph i) (Clos t p ph i)
  | SigTypeSem (Semantic t p ph i) (Clos t p ph i) 
  | PairSem (Semantic t p ph i) (Semantic t p ph i)
  | UniSem UniLevel
  deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal)

-- |We also have to consider the case that something wants to reduce further before becoming a 
-- value but cannot because its blocked on something. These are called neutral terms. The 
-- canonical example of a neutral term is just a variable x. It's not yet a value but there's 
-- no way to convert it to a value since we have no information on what x is yet. Similarly, if 
-- we have some neutral term and we apply Fst to it it's clearly still not a value but we don't 
-- have any way of reducing it further so what's there to do. 
data Ne t p ph i =
    VarSem Atom
  | ApSem (Ne t p ph i) (Nf t p ph i)
  | FstSem (Ne t p ph i)
  | SndSem (Ne t p ph i)
  deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal)

-- |nf is a special class of values coming from the style of NbE we use. It associates a type 
-- with a value so that later during quotation we can eta expand it appropriately
data Nf t p ph i =
    Normal {tpNf :: Semantic t p ph i, termNf :: Semantic t p ph i}
    deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal)

do_fst :: (Bindable t, Bindable p, Bindable i) => Semantic t p ph i -> Semantic t p ph i
do_fst (PairSem p1 _)                   = p1
do_fst (NeutralSem (SigTypeSem t _) ne) = NeutralSem t (FstSem ne)
do_fst _                                = error "Couldn't fst argument in do_fst"

do_snd :: (Bindable t, Bindable p, Bindable i) => Semantic t p ph i -> Semantic () () ph ()
do_snd (PairSem _ p2)                       = undefined--p2
do_snd (NeutralSem p@(SigTypeSem _ clo) ne) = undefined--NeutralSem (do_clos clo (do_fst p)) (SndSem ne)
do_snd _                                    = error "Couldn't snd argument in do_snd"

do_ap :: (Bindable t, Bindable p, Bindable i) => Semantic t p ph i -> Semantic t p ph i -> Semantic () () ph ()
do_ap (LamSem clos)                      a = do_clos clos a
do_ap (NeutralSem (PiTypeSem src dst) e) a = undefined--NeutralSem (do_clos dst a) (ApSem e (Normal src a))
do_ap (NeutralSem _ e)                   a = error "Not a Pi in do_ap"
do_ap _                                  a = error "Not a function in do_ap"


do_clos :: (Bindable t, Bindable p, Bindable i) => (Bindable i, Bindable t, Bindable p)
        => Clos t p ph i     -- ^ The closure to evaluate
        -> Semantic t p ph i -- ^ What to pass to the closure (will be added to the environment)
        -> Semantic () () ph () -- ^ The evaluated closure
do_clos (Clos ((_, (atom, _)) :. term) env) bound = undefined--eval term ((atom, bound) : env)


mk_var :: (Bindable t, Bindable p, Bindable i) => Semantic t p ph i -> Atom -> Semantic t p ph i
mk_var tp atom = NeutralSem tp (VarSem atom)


-- |This converts the surface syntax into a semantic term with no beda redexes.
eval :: (Bindable t, Bindable p, Bindable i, Show t, Show p, Show i) => Surface t p ph i -> SemanticEnv t p ph i -> Semantic () () ph ()
eval (LambdaVariable (_, i) _ _) env =
  case i `lookup` env  of
    Just x -> undefined --x
    _      -> undefined--error ("index " <> show i <> " outide of range of environment: " <> show env)
eval (NatTypeExpression                         _ _) _   = NatTypeSem
eval (NumberLiteral i 0                         _ _) _   = NatValueSem i
eval (AddExpression t1 t2                       _ _) env = AddSem (eval t1 env) (eval t2 env)
eval (TArrowBinding src dest                    _ _) env = undefined--PiTypeSem (eval src env) (Clos dest env)
eval (FunctionLiteralExpression ((t:[]) :. exp) _ _) env = undefined--LamSem (Clos (t :. exp) env)
eval (FunctionLiteralExpression ((t:ts) :. exp) _ _) env = undefined--LamSem (Clos (t :. (FunctionLiteralExpression (ts :. exp) () ())) env)
eval (FunctionApplicationExpression func args   _ _) env = foldl' do_ap (eval func env) $ fmap (flip eval env) args
eval (UniverseExpression  i                     _ _) _   = UniSem i
eval (TSigmaBinding    t1 t2                    _ _) env = undefined--SigTypeSem (eval t1 env) (Clos t2 env)
eval (PairExpression   t1 t2                    _ _) env = PairSem (eval t1 env) (eval t2 env)
eval (FirstExpression  t                        _ _) env = do_fst (eval t env)
eval (SecondExpression t                        _ _) env = do_snd (eval t env)

-- |This is the "quotation" side of the algorithm. 
-- It is a function converting semantic terms back to syntactic ones.
-- These functions are the "read back" functions. We define 3 free forms of read back: 
--   - one for normal forms
--   - one for neutral terms
--   - one for types. 
read_back_nf :: (Bindable t, Bindable p, Bindable i) => Nf t p ph i -> Surface () () ph ()
read_back_nf (Normal (PiTypeSem src dest) f)                 = FunctionLiteralExpression ([((), (atom, ()))] :. (read_back_nf nf)) () ()
                                                            where Clos ((_, (atom, _)) :. _) _ = dest
                                                                  arg = mk_var src atom 
                                                                  nf  = Normal (do_clos dest arg) (do_ap f arg)  
read_back_nf (Normal (SigTypeSem fst snd) p)                 = PairExpression
                                                                  (read_back_nf (Normal fst                      (do_fst p)))
                                                                  (read_back_nf (Normal (do_clos snd (do_fst p)) (do_snd p))) () ()
read_back_nf (Normal NatTypeSem (NatValueSem i))             = NumberLiteral i 0 () ()
read_back_nf (Normal NatTypeSem (AddSem nf1 nf2))            = AddExpression (read_back_nf (Normal NatTypeSem nf1)) (read_back_nf (Normal NatTypeSem nf2)) () ()
read_back_nf (Normal NatTypeSem (NeutralSem _ ne))           = read_back_ne ne
read_back_nf (Normal (UniSem i) (PiTypeSem src dest))        = TArrowBinding
                                                                 (read_back_nf (Normal (UniSem i) src))
                                                                 (((), (atom, ())) :. (read_back_nf (Normal (UniSem i) (do_clos dest var)))) () ()
                                                                where Clos ((_, (atom, _)) :. _) _ = dest
                                                                      var = mk_var src atom
read_back_nf (Normal (NeutralSem _ _) (NeutralSem _ ne)) = read_back_ne ne
read_back_nf  _                                          = error "Ill-typed read_back_nf"

-- |This is almost like the read back for normal forms but deals directly with D.t 
-- so there is no annotation tell us what type we're reading back at. 
-- The function itself just assumes that d is some term of type Uni i for some i. 
-- This, however, means that the cases are almost identical to the type cases in read_back_nf. 
read_back_tp :: (Bindable t, Bindable p, Bindable i) => Semantic t p ph i -> Surface () () ph ()
read_back_tp (NeutralSem _ term) = read_back_ne term
read_back_tp  NatTypeSem             = NatTypeExpression () ()
read_back_tp (PiTypeSem src dest)    = TArrowBinding (read_back_tp src) (((), (atom, ())) :. (read_back_tp (do_clos dest var))) () ()
    where Clos ((_, (atom, _)) :. _) _ = dest
          var = mk_var src atom
read_back_tp (SigTypeSem f s)        = TSigmaBinding (read_back_tp f) (((), (atom, ())) :. (read_back_tp (do_clos s var))) () ()
    where Clos ((_, (atom, _)) :. _) _ = s
          var = mk_var f atom 
read_back_tp (UniSem k)          = UniverseExpression k () ()
read_back_tp  _                  = error "Nbe_failed - Not a type in read_back_tp"

read_back_ne :: (Bindable t, Bindable p, Bindable i) => Ne t p ph i -> Surface () () ph ()
read_back_ne (VarSem x)     = LambdaVariable ((), x) () ()
read_back_ne (ApSem ne arg) = FunctionApplicationExpression (read_back_ne ne) [read_back_nf arg] () ()
read_back_ne (FstSem ne)    = FirstExpression (read_back_ne ne) () ()
read_back_ne (SndSem ne)    = SecondExpression (read_back_ne ne) () ()

-- |The environment is a list of types associated with variables which are supposed to be a member of that type.
-- For each entry we use eval to convert it to a semantic type, tp and then add a neutral term Var i at 
-- type tp where i is the variable at that type. 
-- Notice that we don't need to worry about eta expanding them; all of that will be handled in read back.
make_initial_env [] = []
make_initial_env ((atom, t):env) = (atom, d):env'
  where
    env' = make_initial_env env 
    d    = NeutralSem (eval t env') (VarSem atom)


m @@ n = FunctionApplicationExpression m n () ()
infixl 9 @@

make_lam         f = with_fresh         (\x -> FunctionLiteralExpression ([((), (x, ()))] :. f (LambdaVariable ((), x))))
make_lam_named n f = with_fresh_named n (\x -> FunctionLiteralExpression ([((), (x, ()))] :. f (LambdaVariable ((), x))))

normalize :: (Bindable t, Bindable p, Bindable i) => SurfaceEnv t p ph i -> Surface t p ph i -> Surface t p ph i -> Surface () () ph ()
normalize env term tp = undefined--read_back_nf (Normal tp' term')
  where env'  = undefined--make_initial_env env 
        tp'   = undefined--eval tp env' 
        term' = undefined--eval term env' 
  

-- testterm = (make_lam $ \x -> (make_lam $ \y -> y @@ x)) @@ ((make_lam $ \x -> SuccSurf x) @@ ZeroSurf) @@ (make_lam $ \x -> SuccSurf x)
-- testtype = NatTypeExpression () () 
-- testenv  = []
-- normalized = normalize testenv testterm testtype


listLookup toLookup [] = Nothing
listLookup toLookup ((k, v):xs)
    | k == toLookup = Just v
    | otherwise = listLookup toLookup xs

copyPropagated :: forall i p t m o. (Eq i, Ord i, Bindable i, Bindable p, Bindable t, Nominal t, Nominal m) => Module t p m i -> (forall ph. JustifiedModule t p ph i -> o) -> Either (i, p) o
copyPropagated (Module (ModuleHeader i (Tinydoc text p2) p3) declarations p) f = Map.withMap dUMap (\m -> f <$> dJmapToJustifiedModule m)
  where
    dUMap = UMap.fromList (fmap (\(Declaration i e _) -> (i, e)) declarations)
    dJmapToJustifiedModule :: (Map.Map ph i (Expression t p m i)) -> Either (i, p) (Map.Map ph i (Expression t p (Map.Key ph i) i))
    dJmapToJustifiedModule m = 
      for duped (\e -> forExpr2 e (justifyReferences m))
      where duped = fmap copyIdentifierOntoMetadata m
    justifyReferences :: Map.Map ph i c -> (i, p)->  Either (i, p) (Map.Key ph i)
    justifyReferences map (i, p) = case i `Map.member` map of 
      Nothing -> Left $ (i, p) 
      Just k  -> pure k 

        
       




example :: String
example = "module i `test module` \n\
          \z    = (-3.5)          \n\
          \func = (|x| x) : (Number -> Number)    \n\
          \main = z.func          \n\
          \"


