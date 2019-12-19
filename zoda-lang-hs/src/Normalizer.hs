module Normalizer (normalize, normalizeType) where
import ClassyPrelude hiding (lookup)
import Data.List (lookup)
import Basic 
import Data.Maybe
import Ast
import CopyPropagatedProgram
import qualified Data.Set as Set
import qualified Data.Map.Justified as Map
import qualified Data.Map.Lazy as UMap
import Nominal hiding ((.))
import Data.Void
import Data.Ratio
import Parser
import qualified Data.List.NonEmpty as NonEmpty

type Constraints m = (Bindable m, Ord m, Show m, NominalShow m)

do_fst :: JustifiedModule () () ph m () i' -> Semantic () () (Map.Key ph m) () i' -> Semantic () () (Map.Key ph m) () i'
do_fst _ (PairSem p1 _)                   = p1
do_fst _ (NeutralSem (SigTypeSem t _) ne) = NeutralSem t (FstSem ne)
do_fst _ a                                = error $ "Couldn't fst argument in do_fst - " -- <> show a

do_snd :: (Constraints m) => JustifiedModule () () ph m () i' -> Semantic () () (Map.Key ph m) () i' -> Semantic () () (Map.Key ph m) () i'
do_snd _    (PairSem _ p2)                       = p2
do_snd modu (NeutralSem p@(SigTypeSem _ clo) ne) = NeutralSem (do_clos modu clo (do_fst modu p)) (SndSem ne)
do_snd _    _                                    = error "Couldn't snd argument in do_snd"

do_ap :: (Constraints m) => JustifiedModule () () ph m () i' -> Semantic () () (Map.Key ph m) () i' -> Semantic () () (Map.Key ph m) () i' -> Semantic () () (Map.Key ph m) () i'
do_ap modu (LamSem clos)                      a = do_clos modu clos a
do_ap modu (NeutralSem (PiTypeSem src dst) e) a = NeutralSem (do_clos modu dst a) (ApSem e (Normal src a))
do_ap _    (NeutralSem _ _)                   _ = error "Not a Pi in do_ap"
do_ap _    _                                  _ = error "Not a function in do_ap"

do_add :: JustifiedModule () () ph m () i' -> Semantic () () (Map.Key ph m) () i' -> Semantic () () (Map.Key ph m) () i' -> Semantic () () (Map.Key ph m) () i'
do_add _    (NatValueSem i1)     (NatValueSem i2)   = NatValueSem $ i1 + i2 
do_add _ e1@(NatValueSem _)      (NeutralSem _ ne)  = NeutralSem NatTypeSem (AddSem1 (Normal NatTypeSem e1) ne)
do_add _    (NeutralSem _ ne) e2@(NatValueSem _)    = NeutralSem NatTypeSem (AddSem2 ne (Normal NatTypeSem e2))
do_add _    (NeutralSem _ ne1)   (NeutralSem _ ne2) = NeutralSem NatTypeSem (AddSem3 ne1 ne2)
do_add _    e1                     e2               = error $ "Arguments to do_add not both numbers - " -- <> show e1 <> " and " <> show e2


do_clos :: forall m ph i i'. (Constraints m)
        => JustifiedModule () () ph m () i'
        -> Clos () () (Map.Key ph m) () i'        -- ^ The closure to evaluate
        -> Semantic () () (Map.Key ph m) () i'    -- ^ What to pass to the closure
        -> Semantic () () (Map.Key ph m) () i' -- ^ The evaluated closure
do_clos modu (Clos (atom :. term) env) bound = eval modu term ((atom, bound) : env)


mk_var :: Semantic t p ph i i' -> Atom -> Semantic t p ph i i'
mk_var tp atom = NeutralSem tp (VarSem atom)
 
 
-- |This converts the surface syntax into a semantic term with no beta redexes.
eval :: forall ph m i i'. (Constraints m) => JustifiedModule () () ph m () i' -> Expression () () (Map.Key ph m) () -> SemanticEnv () () (Map.Key ph m) () i' -> Semantic () () (Map.Key ph m) () i'
eval modu = eval'
  where 
    eval' :: Expression () () (Map.Key ph m) () -> SemanticEnv () () (Map.Key ph m) () i' -> Semantic () () (Map.Key ph m) () i'
    eval' (LambdaVariable (i, _) _ _) env =
      case i `lookup` env  of
        Just x -> x
        _      -> error "Couldn't find referenced variable"
    eval' (NatTypeExpression                                       _ _) _   = NatTypeSem
    eval' (NumberLiteral i                                         _ _) _   = if denominator i == 1 then NatValueSem $ numerator i else error "We do not yet support fractional number literals!"
    eval' (AddExpression t1 t2                                     _ _) env = do_add modu (eval' t1 env) (eval' t2 env)
    eval' (TArrowBinding t        _ _)                                  env = evalPi env t
    eval' (FunctionLiteralExpression (LastArg _ (((a, _, _) :. express))) _ _) env = LamSem (Clos (a :. express) env)
    eval' (FunctionLiteralExpression (Arg     _ (((a, _, _) :. moreArg))) _ _) env = LamSem (Clos (a :. (FunctionLiteralExpression moreArg () ())) (env))
    eval' (FunctionApplicationExpression func args                 _ _) env = foldl' (do_ap modu) (eval' func env) $ fmap (flip eval' env) args
    eval' (UniverseExpression  i                                   _ _) _   = UniSem i
    eval' (TSigmaBinding    t1 (Just (a, _, _) :. t2)              _ _) env = SigTypeSem (eval' t1 env) (Clos                    (a :. t2) env)
    eval' (TSigmaBinding    t1 (Nothing :. t2)                     _ _) env = SigTypeSem (eval' t1 env) (Clos (with_fresh $ \a -> a :. t2) env)
    eval' (PairExpression   t1 t2                                  _ _) env = PairSem (eval' t1 env) (eval' t2 env)
    eval' (FirstExpression  t                                      _ _) env = do_fst modu (eval' t env)
    eval' (SecondExpression t                                      _ _) env = do_snd modu (eval' t env)
    eval' (ParenthesizedExpression  t                              _ _) env = eval' t env
    eval' (Annotation  t _                                         _ _) env = eval' t env
    eval' (ReferenceVariable _ m                                   _ _) env = eval' (getValue referent) env
      where referent = m `Map.lookup` modu
            getValue (Value e) = e 
            getValue (ValueAndAnnotation e _) = e 
            getValue (TypeConstructor n t) = error ("Type Constructors not yet supported") `seq` undefined
            getValue (DataConstructor _ t) = error "Data Constructors not yet supported" `seq` undefined

    evalPi env (Scope src ((Just (a, _, _)) :. dest)) =  PiTypeSem (eval' src env) (Clos (a :. (TArrowBinding dest () ())) env)
    evalPi env (Scope src ((Nothing       ) :. dest)) =  PiTypeSem (eval' src env) (Clos ((with_fresh id) :. (TArrowBinding dest () ())) env)
    evalPi env (Pi    src ((Just (a, _, _)) :. dest)) =  PiTypeSem (eval' src env) (Clos (a :.                              dest       ) env)
    evalPi env (Pi    src ((Nothing       ) :. dest)) =  PiTypeSem (eval' src env) (Clos ((with_fresh id) :.                dest       ) env)

    
-- |This is the "quotation" side of the algorithm. 
-- It is a function converting semantic terms back to syntactic ones.
-- These functions are the "read back" functions. We define 3 free forms of read back: 
--   - one for normal forms
--   - one for neutral terms
--   - one for types. 
read_back_nf :: (Constraints m) => JustifiedModule () () ph m () i' ->  Nf () () (Map.Key ph m) () i' -> Expression () () (Map.Key ph m) ()
read_back_nf modu (Normal (PiTypeSem src dest) f)                  = FunctionLiteralExpression (LastArg srcType ((atom, np, np) :. (read_back_nf modu nf))) () ()
                                                                      where Clos (atom :. _) _ = dest
                                                                            srcType = read_back_tp modu src 
                                                                            arg = mk_var src atom 
                                                                            nf  = Normal (do_clos modu dest arg) (do_ap modu f arg)
  
  {-FunctionLiteralExpression ((NonEmpty.fromList [((NoBind (), (atom, NoBind ())), NoBind Nothing)]) :. (read_back_nf modu nf)) () ()
                                                            where Clos (atom :. _) _ = dest
                                                                  arg = mk_var src atom 
                                                                  nf  = Normal (do_clos modu dest arg) (do_ap modu f arg)  -}
read_back_nf modu (Normal (SigTypeSem f s) p)                     = PairExpression
                                                                      (read_back_nf modu (Normal f                                (do_fst modu p)))
                                                                      (read_back_nf modu (Normal (do_clos modu s (do_fst modu p)) (do_snd modu p))) () ()
read_back_nf _    (Normal NatTypeSem (NatValueSem i))             = NumberLiteral (fromInteger i) () ()
read_back_nf modu (Normal NatTypeSem (NeutralSem _ ne))           = read_back_ne modu ne
read_back_nf modu (Normal (UniSem i) (PiTypeSem src dest))        = TArrowBinding (Pi (read_back_nf modu (Normal (UniSem i) src)) ((Just (atom, np, np))  :. (read_back_nf modu (Normal (UniSem i) (do_clos modu dest var))))) () ()
                                                                      where Clos (atom :. _) _ = dest
                                                                            var = mk_var src atom
read_back_nf modu (Normal (UniSem i) (SigTypeSem fst snd)) = TSigmaBinding (read_back_nf modu (Normal (UniSem i) fst)) (Just (atom, np, np) :. read_back_nf modu (Normal (UniSem i) (do_clos modu snd var))) () ()
  where Clos (atom :. _) _ = snd
        var = mk_var fst atom
read_back_nf _    (Normal (UniSem _) (UniSem j)) = UniverseExpression j () ()
read_back_nf _    (Normal (UniSem _) NatTypeSem) = NatTypeExpression () ()
read_back_nf modu (Normal (NeutralSem _ _) (NeutralSem _ ne)) = read_back_ne modu ne
read_back_nf _     v                                          = error $ "Ill-typed read_back_nf - " <> show v

-- |This is almost like the read back for normal forms but works specifically for types
-- so there is no annotation tell us what type we're reading back at. 
-- The function itself just assumes that d is some term of type Uni i for some i. 
-- This, however, means that the cases are almost identical to the type cases in read_back_nf. 
read_back_tp :: forall m ph i i'. (Constraints m) => JustifiedModule () () ph m () i' -> Semantic () () (Map.Key ph m) () i' -> Expression () () (Map.Key ph m) ()
read_back_tp modu (NeutralSem _ term)  = read_back_ne modu term
read_back_tp _    NatTypeSem           = NatTypeExpression () ()
read_back_tp modu (PiTypeSem src dest) = TArrowBinding (Pi (read_back_tp modu src) ((Just (atom, np, np)) :. (read_back_tp modu (do_clos modu dest var)))) () ()
    where Clos (atom :. _) _ = dest
          var = mk_var src atom
read_back_tp modu (SigTypeSem f s)     = TSigmaBinding (read_back_tp modu f) (Just (atom, np, np) :. (read_back_tp modu (do_clos modu s var))) () ()
    where Clos (atom :. _) _ = s
          var = mk_var f atom 
read_back_tp _ (UniSem k)              = UniverseExpression k () ()
read_back_tp _ _                       = error "Nbe_failed - Not a type in read_back_tp"

read_back_ne :: forall m ph i i'.  (Constraints m) => JustifiedModule () () ph m () i' -> Ne () () (Map.Key ph m) () i' -> Expression () () (Map.Key ph m) ()
read_back_ne _    (VarSem x)        = LambdaVariable (x, ()) () ()
read_back_ne modu (ApSem ne arg)    = FunctionApplicationExpression (read_back_ne modu ne) (read_back_nf modu arg NonEmpty.:| []) () ()
read_back_ne modu (FstSem ne)       = FirstExpression (read_back_ne modu ne) () ()
read_back_ne modu (SndSem ne)       = SecondExpression (read_back_ne modu ne) () ()
read_back_ne modu (AddSem1 nf ne)   = AddExpression (read_back_nf modu nf) (read_back_ne modu ne) () ()
read_back_ne modu (AddSem2 ne nf)   = AddExpression (read_back_ne modu ne) (read_back_nf modu nf) () ()
read_back_ne modu (AddSem3 ne1 ne2) = AddExpression (read_back_ne modu ne1) (read_back_ne modu ne2) () ()


{- ice!
type T ph = [ph]
err :: forall ph i. T ph i -> Int
err = undefined :: ph
-}

make_initial_env :: forall ph m i i'. (Constraints m) => JustifiedModule () () ph m () i' -> [(Atom, Expression () () (Map.Key ph m) ())] -> SemanticEnv () () (Map.Key ph m) () i'
make_initial_env _    [] = []
make_initial_env modu ((atom, t):env) = (atom, d):env'
  where
    env' :: SemanticEnv () () (Map.Key ph m) () i'
    env' = make_initial_env modu env 
    d :: Semantic () () (Map.Key ph m) () i'
    d = eval modu (normalizeExprMetadata t) env'

{-

make_initial_env :: forall ph m i i'. (Constraints m) => JustifiedModule () () ph m () i i' -> [(Atom, Expression () () (Map.Key ph m) ())] -> SemanticEnv () () (Map.Key ph m) () i'
make_initial_env _    [] = []
make_initial_env modu ((atom, t):env) = (atom, d):env'
  where
    env' :: SemanticEnv () () (Map.Key ph m) () i'
    env' = make_initial_env modu env 
    d :: Semantic () () (Map.Key ph m) () i'
    d = eval modu (normalizeExprMetadata t) env'


-}

(@@) :: Expression () () m i -> (NonEmpty.NonEmpty (Expression () () m i)) -> Expression () () m i
m @@ n = FunctionApplicationExpression m n () ()
infixl 9 @@

normalize :: forall t p i i' ph m. (Bindable t, Bindable p, Bindable i, Constraints m) => JustifiedModule t p ph m i i' -> SurfaceEnv t p (Map.Key ph m) i -> Expression t p (Map.Key ph m) i -> Expression t p (Map.Key ph m) i -> Expression () () (Map.Key ph m) ()
normalize modu env term tp = read_back_nf modu' (Normal tp' term')
  where env'  :: SemanticEnv () () (Map.Key ph m) () i'
        env'  = make_initial_env modu' (normalizeExprEnv env) 
        tp' :: Semantic () () (Map.Key ph m) () i'
        tp' = eval modu' (normalizeExprMetadata tp) env' 
        term' :: Semantic () () (Map.Key ph m) () i'
        term' = eval modu' (normalizeExprMetadata term) env' 
        modu' = fmap normalizeDelcarationInfoMetadata modu


normalizeType :: forall t p i i' ph m. (Bindable t, Bindable p, Bindable i, Constraints m) => JustifiedModule t p ph m i i' -> SurfaceEnv t p (Map.Key ph m) i -> Expression t p (Map.Key ph m) i -> Expression () () (Map.Key ph m) ()
normalizeType modu env term = read_back_tp modu' term'
  where env'  :: SemanticEnv () () (Map.Key ph m) () i'
        env'  = make_initial_env modu' (normalizeExprEnv env) 
        term' :: Semantic () () (Map.Key ph m) () i'
        term' = eval modu' (normalizeExprMetadata term) env' 
        modu' = fmap normalizeDelcarationInfoMetadata modu



np = NoBind ()