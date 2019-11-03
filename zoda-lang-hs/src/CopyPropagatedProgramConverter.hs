module CopyPropagatedProgramConverter where
import ClassyPrelude hiding (lookup)
import Data.List (lookup)
import Basic
import Data.Maybe
import Ast
import qualified CopyPropagatedProgram as CPP
import Capability.Error
import qualified Data.Set as Set
import Nominal hiding ((.))

data Metavariable = TypechecksOkay{-MetavariableApplication Int [Metavariable] 
                  | Metavariable Int 
                  | Constant Int-}

data Types i = Number | Bool | Arr (Maybe i) (Types i) (Types i) deriving (Eq, Show, Read)


--data IdentifierMeaning t p i = Ref (Expression t p (i, Maybe (IdentifierMeaning t p i))) | LambdaVar | NotAReference deriving (Eq, Ord, Read, Show)

type UniLevel = Int
data Surface = 
    VarSurf Atom

  -- nats
  | NatSurf
  | ZeroSurf
  | SuccSurf Surface
  -- nrec goes here
  
  -- functions 
  | PiSurf Surface (Bind Atom Surface)
  | LamSurf (Bind Atom Surface)
  | ApSurf Surface Surface

  -- pairs 
  | SigSurf Surface (Bind Atom Surface)
  | PairSurf Surface Surface
  | FstSurf Surface 
  | SndSurf Surface
  
  -- universes 
  | UniSurf UniLevel
  deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal)

type SurfaceEnv = [(Atom, Surface)]


type SemanticEnv = [(Atom, Semantic)]
data Clos    = Clos {termClos :: (Bind Atom Surface), envClos :: SemanticEnv}
  deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal)

-- |Semantic values contain terms which have evaluated to a constructor which does not need to be 
-- reduced further. So for instance, Lam and Pair may contain computation further inside the term 
-- but at least the outermost constructor is stable and fully evaluated. 
data Semantic =
    LamSem Clos
  | NeutralSem {tpNeutral :: Semantic,  -- ^This should be the type of the neutral term
                termNeutral :: Ne}      -- ^This should be the neutral term iteslf
  | NatSem
  | ZeroSem
  | SuccSem Semantic
  | PiSem Semantic Clos
  | SigSem Semantic Clos 
  | PairSem Semantic Semantic
  | UniSem UniLevel
  deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal)

-- |We also have to consider the case that something wants to reduce further before becoming a 
-- value but cannot because its blocked on something. These are called neutral terms. The 
-- canonical example of a neutral term is just a variable x. It's not yet a value but there's 
-- no way to convert it to a value since we have no information on what x is yet. Similarly, if 
-- we have some neutral term and we apply Fst to it it's clearly still not a value but we don't 
-- have any way of reducing it further so what's there to do. 
data Ne =
    VarSem Atom
  | ApSem Ne Nf
  | FstSem Ne
  | SndSem Ne
  deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal)

-- |nf is a special class of values coming from the style of NbE we use. It associates a type 
-- with a value so that later during quotation we can eta expand it appropriately
data Nf =
    Normal {tpNf :: Semantic, termNf :: Semantic}
    deriving (Show, Eq, NominalSupport, NominalShow, Generic, Nominal)

do_fst :: Semantic -> Semantic
do_fst (PairSem p1 _)                  = p1
do_fst (NeutralSem (SigSem t _) ne)    = NeutralSem t (FstSem ne)
do_fst _                               = error "Couldn't fst argument in do_fst"

do_snd :: Semantic -> Semantic
do_snd (PairSem _ p2)                    = p2
do_snd (NeutralSem p@(SigSem _ clo) ne)  = NeutralSem (do_clos clo (do_fst p)) (SndSem ne)
do_snd _                                 = error "Couldn't snd argument in do_snd"

do_ap   :: Semantic -> Semantic -> Semantic
do_ap (LamSem clos)                  a = do_clos clos a
do_ap (NeutralSem (PiSem src dst) e) a = NeutralSem (do_clos dst a) (ApSem e (Normal src a))
do_ap (NeutralSem _ e)               a = error "Not a Pi in do_ap"
do_ap _                              a = error "Not a function in do_ap"


do_clos :: Clos     -- ^ The closure to evaluate
        -> Semantic -- ^ What to pass to the closure (will be added to the environment)
        -> Semantic -- ^ The evaluated closure
do_clos (Clos (atom :. term) env) bound = eval term ((atom, bound) : env)


mk_var :: Semantic -> Atom -> Semantic
mk_var tp atom = NeutralSem tp (VarSem atom)


-- |This converts the surface syntax into a semantic term with no beda redexes.
eval :: Surface -> SemanticEnv -> Semantic
eval (VarSurf i) env =
  case i `lookup` env  of
    Just x -> x
    _      -> error ("index " <> show i <> " outide of range of environment: " <> show env)
eval  NatSurf            _   = NatSem
eval  ZeroSurf           _   = ZeroSem
eval (SuccSurf i)        env = SuccSem (eval i env)
eval (PiSurf   src dest) env = PiSem (eval src env) (Clos dest env)
eval (LamSurf  t)        env = LamSem (Clos t env)
eval (ApSurf   t1  t2)   env = do_ap (eval t1 env) (eval t2 env)
eval (UniSurf  i)        _   = UniSem i
eval (SigSurf  t1  t2)   env = SigSem (eval t1 env) (Clos t2 env)
eval (PairSurf t1  t2)   env = PairSem (eval t1 env) (eval t2 env)
eval (FstSurf  t)        env = do_fst (eval t env)
eval (SndSurf  t)        env = do_snd (eval t env)

-- |This is the "quotation" side of the algorithm. 
-- It is a function converting semantic terms back to syntactic ones.
-- These functions are the "read back" functions. We define 3 free forms of read back: 
--   - one for normal forms
--   - one for neutral terms
--   - one for types. 
read_back_nf :: Nf -> Surface
read_back_nf (Normal (PiSem src dest) f)                 = LamSurf (atom :. (read_back_nf nf))
                                                            where Clos (atom :. _) _ = dest
                                                                  arg = mk_var src atom 
                                                                  nf  = Normal (do_clos dest arg) (do_ap f arg)  
read_back_nf (Normal (SigSem fst snd) p)                 = PairSurf
                                                                  (read_back_nf (Normal fst                      (do_fst p)))
                                                                  (read_back_nf (Normal (do_clos snd (do_fst p)) (do_snd p)))
read_back_nf (Normal NatSem  ZeroSem)                    = ZeroSurf
read_back_nf (Normal NatSem (SuccSem nf))                = SuccSurf (read_back_nf (Normal NatSem nf))
read_back_nf (Normal NatSem (NeutralSem _ ne))           = read_back_ne ne
read_back_nf (Normal (UniSem i) (PiSem src dest))        = PiSurf
                                                              (read_back_nf (Normal (UniSem i) src))
                                                              (atom :. (read_back_nf (Normal (UniSem i) (do_clos dest var))))
                                                            where Clos (atom :. _) _ = dest
                                                                  var = mk_var src atom
read_back_nf (Normal (NeutralSem _ _) (NeutralSem _ ne)) = read_back_ne ne
read_back_nf  _                                          = error "Ill-typed read_back_nf"

-- |This is almost like the read back for normal forms but deals directly with D.t 
-- so there is no annotation tell us what type we're reading back at. 
-- The function itself just assumes that d is some term of type Uni i for some i. 
-- This, however, means that the cases are almost identical to the type cases in read_back_nf. 
read_back_tp :: Semantic -> Surface
read_back_tp (NeutralSem _ term) = read_back_ne term
read_back_tp  NatSem             = NatSurf
read_back_tp (PiSem src dest)    = PiSurf (read_back_tp src) (atom :. (read_back_tp (do_clos dest var)))
    where Clos (atom :. _) _ = dest
          var = mk_var src atom
read_back_tp (SigSem f s)        = SigSurf (read_back_tp f) (atom :. (read_back_tp (do_clos s var)))
    where Clos (atom :. _) _ = s
          var = mk_var f atom 
read_back_tp (UniSem k)          = UniSurf k
read_back_tp  _                  = error "Nbe_failed - Not a type in read_back_tp"

read_back_ne :: Ne -> Surface
read_back_ne (VarSem x)     = VarSurf x
read_back_ne (ApSem ne arg) = ApSurf (read_back_ne ne) (read_back_nf arg)
read_back_ne (FstSem ne)    = FstSurf (read_back_ne ne)
read_back_ne (SndSem ne)    = SndSurf (read_back_ne ne)

-- |The environment is a list of types associated with variables which are supposed to be a member of that type.
-- For each entry we use eval to convert it to a semantic type, tp and then add a neutral term Var i at 
-- type tp where i is the variable at that type. 
-- Notice that we don't need to worry about eta expanding them; all of that will be handled in read back.
make_initial_env [] = []
make_initial_env ((atom, t):env) = (atom, d):env'
  where
    env' = make_initial_env env 
    d    = NeutralSem (eval t env') (VarSem atom)


m @@ n = ApSurf m n
infixl 9 @@

make_lam         f = with_fresh         (\x -> LamSurf (x :. f (VarSurf x)))
make_lam_named n f = with_fresh_named n (\x -> LamSurf (x :. f (VarSurf x)))

normalize :: SurfaceEnv -> Surface -> Surface -> Surface
normalize env term tp = read_back_nf (Normal tp' term')
  where env'  = make_initial_env env 
        tp'   = eval tp env' 
        term' = eval term env' 
  

testterm = (make_lam $ \x -> (make_lam $ \y -> y @@ x)) @@ ((make_lam $ \x -> SuccSurf x) @@ ZeroSurf) @@ (make_lam $ \x -> SuccSurf x)
testtype = NatSurf 
testenv  = []
normalized = normalize testenv testterm testtype


listLookup toLookup [] = Nothing
listLookup toLookup ((k, v):xs)
    | k == toLookup = Just v
    | otherwise = listLookup toLookup xs


{-
checkNamesDefined :: forall t p i m. (Ord i, IsString i, Show i, Show t, Ord t, Show p, Eq p, HasThrow "perr" (ProductionError t p i) m) => Module t p i -> m (Module t p (i, Maybe (IdentifierMeaning t p i)))
checkNamesDefined = copyPropagate
  where

    copyPropagate ::  Module t p i -> m (Module t p (i, Maybe (IdentifierMeaning t p i)))
    copyPropagate (Module (ModuleHeader (Identifier i1 p1) (Tinydoc text p2) p3) declarations p4) = do
      allTopLevelTypechecks <- mapM ((synth []) . \case (_, Ref x) -> x) allTopLevelValues
      pure $ traceShow allTopLevelTypechecks $ Module (ModuleHeader (Identifier (i1, Just NotAReference) p1) (Tinydoc text p2) p3) propagatedDeclarations p4
      where 
        allTopLevelValues = map (\case d@(Declaration (Identifier i _) e _) -> (i, Ref (propagateExpression allTopLevelValues e))) declarations
        

        propagatedDeclarations ::  [Declaration t p (i, Maybe (IdentifierMeaning t p i))]
        propagatedDeclarations = map (propagateDeclaration allTopLevelValues) declarations
        
        propagateDeclaration :: [(i, IdentifierMeaning t p i)] -> Declaration t p i -> Declaration t p (i, Maybe (IdentifierMeaning t p i))
        propagateDeclaration context (Declaration (Identifier i p1) expression p2) = 
          Declaration (Identifier (i, Just NotAReference) p1) propagatedExpression p2
            where 
              propagatedExpression :: Expression t p (i, Maybe (IdentifierMeaning t p i))
              propagatedExpression = propagateExpression context expression
-}



example :: String
example = "module i `test module` \n\
          \z    = (-3.5)          \n\
          \func = (|x| x) : (Number -> Number)    \n\
          \main = z.func          \n\
          \"


