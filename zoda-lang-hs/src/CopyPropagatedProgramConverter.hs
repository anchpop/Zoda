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
import Data.Void

data Metavariable = TypechecksOkay{-MetavariableApplication Int [Metavariable] 
                  | Metavariable Int 
                  | Constant Int-}

data Types i = Number | Bool | Arr (Maybe i) (Types i) (Types i) deriving (Eq, Show, Read)





do_fst :: (Bindable t, Bindable p, Bindable i, Nominal m) => Semantic t p m i -> Semantic t p m i
do_fst (PairSem p1 _)                   = p1
do_fst (NeutralSem (SigTypeSem t _) ne) = NeutralSem t (FstSem ne)
do_fst _                                = error "Couldn't fst argument in do_fst"

do_snd :: (Bindable t, Bindable p, Bindable i, Nominal m) => Semantic t p m i -> Semantic t p m i
do_snd (PairSem _ p2)                       = p2
do_snd (NeutralSem p@(SigTypeSem _ clo) ne) = NeutralSem (do_clos clo (do_fst p)) (SndSem ne)
do_snd _                                    = error "Couldn't snd argument in do_snd"

do_ap :: (Bindable t, Bindable p, Bindable i, Nominal m) => Semantic t p m i -> Semantic t p m i -> Semantic t p m i
do_ap (LamSem clos)                      a = do_clos clos a
do_ap (NeutralSem (PiTypeSem src dst) e) a = NeutralSem (do_clos dst a) (ApSem e (Normal src a))
do_ap (NeutralSem _ e)                   a = error "Not a Pi in do_ap"
do_ap _                                  a = error "Not a function in do_ap"


do_clos :: (Bindable t, Bindable p, Bindable i, Nominal m) => (Bindable i, Bindable t, Bindable p)
        => Clos t p m i     -- ^ The closure to evaluate
        -> Semantic t p m i -- ^ What to pass to the closure (will be added to the environment)
        -> Semantic t p m i -- ^ The evaluated closure
do_clos (Clos ((_, (atom, _)) :. term) env) bound = undefined--eval term ((atom, bound) : env)


mk_var :: (Bindable t, Bindable p, Bindable i) => Semantic t p ph i -> Atom -> Semantic t p ph i
mk_var tp atom = NeutralSem tp (VarSem atom)


-- |This converts the surface syntax into a semantic term with no beda redexes.
eval :: (Bindable t, Bindable p, Bindable i, Show t, Show p, Show i, Nominal m) => JustifiedExpression t p m i -> SemanticEnv t p m i -> Semantic t p m i
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
read_back_nf :: (Bindable t, Bindable p, Bindable i, Nominal m) => Nf t p m i -> Expression () () () ()
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
read_back_tp :: (Bindable t, Bindable p, Bindable i, Nominal m) => Semantic t p m i -> Expression () () () ()
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

read_back_ne :: (Bindable t, Bindable p, Bindable i, Nominal m) => Ne t p m i -> Expression () () () ()
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

normalize :: (Bindable t, Bindable p, Bindable i) => SurfaceEnv t p ph i -> JustifiedExpression t p ph i -> JustifiedExpression t p ph i -> JustifiedExpression () () ph ()
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
    dJmapToJustifiedModule :: (Map.Map ph i (Expression t p m i)) -> Either (i, p) (Map.Map ph i (JustifiedExpression t p ph i))
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


