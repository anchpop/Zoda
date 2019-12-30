module Typechecker where
import ClassyPrelude hiding (try, many)
import Nominal hiding ((.))
import Ast
import Basic
import Data.List.NonEmpty( NonEmpty( (:|) ) )
import Normalizer
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Justified as Map
import qualified Data.Semigroup as Semigroup


data Env ph i m = Env {types :: [(Atom, JustifiedExpressionX Parsed ph i m)], values :: [(Atom, JustifiedExpressionX Parsed ph i m)]} 
envToPlain = fmap (\(a, expParsed) -> (a, toPlain expParsed)) 

deriving instance (ConstraintX Generic Parsed i (Map.Key ph m)) => Generic (Env ph i m)
deriving instance (ConstraintX Nominal Parsed i (Map.Key ph m), ConstraintX Eq Parsed i (Map.Key ph m), Eq i, Nominal i, Eq m, Nominal m) => Eq (Env ph i m)
deriving instance (ConstraintX NominalShow Parsed i (Map.Key ph m), ConstraintX Show Parsed i (Map.Key ph m), ConstraintX Show Parsed i m, Show i, NominalShow i, Show m, NominalShow m) => Show (Env ph i m)


addTypeToEnv :: Atom -> JustifiedExpressionX Parsed ph i m -> Env ph i m -> Env ph i m
addTypeToEnv a t (Env ts vs) = (Env ((a, t):ts) vs)
addValueToEnv :: Atom -> JustifiedExpressionX Parsed ph i m -> Env ph i m -> Env ph i m
addValueToEnv a v (Env ts vs) = (Env ts ((a, v):vs))
lookupTypeInEnv :: Atom -> Env ph i m -> Maybe (JustifiedExpressionX Parsed ph i m)
lookupTypeInEnv a (Env ts _) = a `lookup` ts
getValueMap :: Env ph i m -> [(Atom, JustifiedExpressionX Parsed ph i m)]
getValueMap (Env _ vs) = vs
instance Semigroup (Env ph i m) where
  (Env t1 v1) <> (Env t2 v2) = Env (t1 <> t2) (v1 <> v2)
instance Monoid (Env ph i m) where
  mempty = Env [] [] 

typecheck modu = case traverse typecheckDelcaration modu of
  Left () -> Left TypeErr
  Right _ -> Right ()
  where typecheckDelcaration (Value v)                = inferType modu mempty v *> pure ()
        typecheckDelcaration (ValueAndAnnotation v t) = checkType modu mempty v t
        typecheckDelcaration (TypeConstructor _ _)    = pure () -- TODO: Check type is well formed
        typecheckDelcaration (DataConstructor _ _)    = pure () -- TODO: Check type is well formed


isSubtype :: forall t p i m n ph. Constraints' ph i m n => JustifiedModule Parsed ph i m n -> Env ph i m -> JustifiedExpressionX Parsed ph i m -> JustifiedExpressionX Parsed ph i m -> Bool
isSubtype modu context e1 e2 = case (e1Normalized, e2Normalized) of 
    (UniverseExpressionX _ uni1, UniverseExpressionX _ uni2) -> uni1 <= uni2
    -- probably should do something with functions and pairs here
    -- but not going to stress it too much since it might change soon
    _                                                          -> e1Normalized == e2Normalized
  where e1Normalized = normalizeType (fmap toPlainDecl modu) (envToPlain $ getValueMap context) (toPlain e1) 
        e2Normalized = normalizeType (fmap toPlainDecl modu) (envToPlain $ getValueMap context) (toPlain e2) 



data MergedFlitAndScope i m = FlitArg (ExpressionX Parsed i m) (ExpressionX Parsed i m) (Bind Atom (MergedFlitAndScope i m)) 
                            | PiArg   (ExpressionX Parsed i m) (ExpressionX Parsed i m) (Bind Atom (ExpressionX Parsed i m, ExpressionX Parsed i m)) 

deriving instance (ConstraintX Generic Parsed i m) => Generic (MergedFlitAndScope i m)
deriving instance (ConstraintX Nominal Parsed i m, ConstraintX Eq Parsed i m, Eq i, Nominal i, Eq m, Nominal m) => Eq (MergedFlitAndScope i m)
deriving instance (ConstraintX NominalShow Parsed i m, ConstraintX Show Parsed i m, Show i, NominalShow i, Show m, NominalShow m) => Show (MergedFlitAndScope i m)
deriving instance (ConstraintX Nominal Parsed i m, Nominal i, Nominal m) => Nominal (MergedFlitAndScope i m)
deriving instance (ConstraintX NominalShow Parsed i m, NominalShow i, NominalShow m) => NominalShow (MergedFlitAndScope i m)
deriving instance (ConstraintX NominalSupport Parsed i m, NominalSupport i, NominalSupport m) => NominalSupport (MergedFlitAndScope i m)



type Constraints i m = (Bindable m, Bindable i, Eq m, Eq i, Ord m, Show m, NominalShow m, Show i, NominalShow i)
type Constraints' ph i m n = (ConstraintX Show Parsed i (Map.Key ph m), ConstraintX Bindable Plain () (Map.Key ph m),  ConstraintX Bindable Plain i (Map.Key ph m), Bindable m, Bindable i, Bindable n, Eq m, Eq i, Ord m, Show m, NominalShow m, Show i, NominalShow i, Show n, NominalShow n, Ord i, Ord n)

mergedFlitAndScope :: forall t p i m. Constraints i m => FunctionLiteralX Parsed i m -> TelescopeX Parsed i m -> Either () (MergedFlitAndScope i m)
mergedFlitAndScope (Arg ea ((aa, _) :. argsInner)) (Scope es (Just (as, _) :. scopeInner)) = do 
  let (a' :. (argsMergedInner, scopeMergedInner)) = merge (aa :. argsInner) (as :. scopeInner)
  mergedInner <- mergedFlitAndScope argsMergedInner scopeMergedInner
  pure $ FlitArg ea es (a'  :. mergedInner)

mergedFlitAndScope (Arg ea ((aa, _) :. argsInner)) (Scope es (Nothing :. scopeInner)) = do
  mergedInner <- mergedFlitAndScope argsInner scopeInner
  pure $ FlitArg ea es (aa :. mergedInner)

mergedFlitAndScope (LastArg ea ((aa, _) :. body)) (Pi es ((Just (as, _)) :. outputType)) = do
  let (a' :. (bodyMerged, outputTypedMerged)) = merge (aa :. body) (as :. outputType)
  pure $ PiArg ea es (a' :. (bodyMerged, outputTypedMerged))

mergedFlitAndScope (LastArg ea ((aa, _) :. body)) (Pi es (Nothing :. outputType)) = do
  pure $ PiArg ea es (aa :. (body, outputType))

mergedFlitAndScope (LastArg _ _) (Scope _ _) = Left () 

mergedFlitAndScope (Arg _ _) (Pi _ _) = Left ()

-- |Takes a context, a list of args, and a telescope.
-- It checks that the input arguments' types match the types specified in 
-- the telescope. It then returns the output type with the appropriate substitutions done.
-- if you pass `Int, 3` to `(a: Type, v: a) -> a` it should return `Int`
checkScopeAndGetOutput :: forall i m n ph. Constraints' ph i m n => JustifiedModule Parsed ph i m n -> Env ph i m -> NonEmpty (JustifiedExpressionX Parsed ph i m) -> JustifiedTelescopeX Parsed ph i m -> Either () (JustifiedExpressionX Parsed ph i m)
checkScopeAndGetOutput modu = checkScopeAndGetOutput'
  where
    checkScopeAndGetOutput' context (arg NonEmpty.:| []) (Pi argType ((Just (a, _)) :. bodyType)) = do 
      checkType modu context arg argType
      pure $ substExpr a arg bodyType
    checkScopeAndGetOutput' context (arg NonEmpty.:| []) (Pi argType (Nothing :. bodyType)) = do 
      checkType modu context arg argType
      pure bodyType 
    checkScopeAndGetOutput' context (arg NonEmpty.:| (r':rest)) (Scope argType ((Just (a, _)) :. innerScope)) = do 
      checkType modu context arg argType
      checkScopeAndGetOutput' context (r' NonEmpty.:| rest) (substTelescope a arg innerScope)
    checkScopeAndGetOutput' context (arg NonEmpty.:| (r':rest)) (Scope argType (Nothing :. innerScope)) = do 
      checkType modu context arg argType
      checkScopeAndGetOutput' context (r' NonEmpty.:| rest) innerScope
    checkScopeAndGetOutput' _ (_ NonEmpty.:| []) (Scope _ _) = Left ()
    checkScopeAndGetOutput' _ (_ NonEmpty.:| _) (Pi _ _) = Left () 

inferType :: forall ph i m n. Constraints' ph i m n => JustifiedModule Parsed ph i m n -> Env ph i m -> JustifiedExpressionX Parsed ph i m -> Either () (JustifiedExpressionX Parsed ph i m)
inferType modu = inferType' where
  inferType' :: Env ph i m -> JustifiedExpressionX Parsed ph i m -> Either () (JustifiedExpressionX Parsed ph i m)
  -- Typing rule for universes
  --     ---------------------------------------
  --           Universe i => Universe (i + 1)
  inferType' _ (UniverseExpressionX inf i) = pure $ UniverseExpressionX inf (i + 1)
  -- Typing rule for Nat
  --     ---------------------------------------
  --           Nat => Universe 1
  inferType' _ (NatTypeExpressionX inf) = pure $ UniverseExpressionX inf 1 
  -- Typing rule for annotations
  --        t is a type      context ⊢ e <= t
  --     ---------------------------------------
  --         context ⊢ (Annotation e t) => t
  inferType' context (AnnotationX _ expr typ) = checkType modu context expr typ *> pure typ -- TODO: also check that it's a valid type
  -- Typing rule for variables
  --            x : t ∈ context
  --     -----------------------------------------
  --           context ⊢ x => t
  inferType' context (LambdaVariableX _ (x, _)) = 
    case x `lookupTypeInEnv` context of
      Just t -> pure t
      _      -> Left ()
  -- Typing rule for function application
  --      context ⊢ func => (Pi inT (x.outT))  context |- inT <= Type     context ⊢ in <= inT
  --     --------------------------------------------------------------------------------------
  --               context ⊢ (App func in) => (outT[in/x]) 
  inferType' context (FunctionApplicationExpressionX _ func args) = do
    funcType <- inferType' context func 
    case funcType of 
      TArrowBindingX _ scope -> checkScopeAndGetOutput modu context args scope
      _                      -> Left ()
  -- Typing rule for functions
  --               context, in : inT ⊢ body => bodyT
  --     ---------------------------------------------------------
  --             context ⊢ (in : inT).body => inT -> bodyT
  inferType' context (FunctionLiteralExpressionX inf flit) = do 
    scope <- makeTelescope context flit
    pure $ TArrowBindingX inf scope
    where makeTelescope context' (Arg     e ((a, i) :. rest)) = do rest' <- makeTelescope (addTypeToEnv a e context') rest
                                                                   pure $ Scope e (Just (a, i) :. rest')  
          makeTelescope context' (LastArg e ((a, i) :. body)) = do bodyType <- inferType' (addTypeToEnv a e context') body
                                                                   pure $ Pi e (Just (a, i) :. bodyType)
  --         context ⊢ x1 <= Nat   context ⊢ x2 <= Nat
  --     -------------------------------------------------------
  --             context ⊢ x1 + x2 => Nat
  inferType' context (AddExpressionX inf e1 e2) = do 
    checkType modu context e1 (NatTypeExpressionX inf) 
    checkType modu context e2 (NatTypeExpressionX inf) 
    pure $ NatTypeExpressionX inf
  --     ---------------------------------------------------------
  --             context ⊢ (1 | 2 | 3 | ...) => Nat
  inferType' _ (NumberLiteralX inf _) = pure $ NatTypeExpressionX inf
  -- Typing rule for parentheses
  --          context ⊢  x => t
  --     -----------------------------
  --          context ⊢ (x) => t
  inferType' context (ParenthesizedExpressionX _ e) = (inferType' context e) 
  inferType' context (TSigmaBindingX _ e1 (Just (a, _) :. e2)) = do
    e1T <- inferType' context e1
    e2T <- inferType' (addTypeToEnv a e1 context) e2
    combineUniverses e1T e2T 
  inferType' context (TSigmaBindingX _ e1 (Nothing :. e2)) = do
    e1T <- inferType' context e1
    e2T <- inferType' context e2
    combineUniverses e1T e2T
  inferType' context (ReferenceVariableX _ _ m) = case m `Map.lookup` modu of 
    Value v                -> inferType' context v
    ValueAndAnnotation _ t -> pure t 
    TypeConstructor    _ t -> pure t 
    DataConstructor    _ t -> pure t 
  inferType' context (FirstExpressionX _ e) = do 
    eT <- inferType' context e 
    case eT of 
      TSigmaBindingX _ fstT _ -> pure fstT
      _                       -> Left ()
  inferType' context (SecondExpressionX _ e) = do
    eT <- inferType' context e 
    case eT of 
      TSigmaBindingX _ e1 (Just (a, _) :. e2) -> pure (substExpr a e1 e2) 
      TSigmaBindingX _ _  (Nothing        :. e2) -> pure e2
      _                                          -> Left ()
  inferType' context (PairExpressionX inf e1T e2T) = do
    e1TT <- inferType' context e1T 
    e2TT <- inferType' context e2T 
    pure $ TSigmaBindingX inf e1TT (Nothing :. e2TT)
  inferType' context (TArrowBindingX _ flit) = combineArrow flit
    where 
      combineArrow (Scope argT (_ :. rest)) = do 
        argTT <- inferType' context argT
        restT <- combineArrow rest
        combineUniverses argTT restT
      combineArrow (Pi argT (_ :. outT)) = do -- TODO: We need to check that outT is greater than inT because
        argTT <- inferType' context argT      --       apparently the codomain has to be higher than the domain
        outTT <- inferType' context outT
        combineUniverses argTT outTT
  inferType' _ other = error $ "couldn't infer a type for " <> show other 

checkType :: Constraints' ph i m n => JustifiedModule Parsed ph i m n -> Env ph i m -> JustifiedExpressionX Parsed ph i m -> JustifiedExpressionX Parsed ph i m -> Either () ()
checkType modu = checkType' 
  where 
    -- Type checking rule for lambda abstraction
    --          context, x:t ⊢ e <= s
    --     ------------------------------------
    --       context ⊢  Lam t (x.e) <= Pi t (x.s)
    -- note that the two `x`s in the binders have to actually be the "same" to correctly type it
    -- since `x` might be mentioned in both `e` and `s`
    -- the `merge` function in nominal is used for this
    -- of couse, because we use telescopes instead of pi types it is a trickier
    checkType' context (FunctionLiteralExpressionX _ flit) shouldBe = 
      case shouldBe of 
        TArrowBindingX _ scope -> do mergedFlitScope <- mergedFlitAndScope flit scope
                                     handleMerge context mergedFlitScope
        _                      -> Left ()
      where handleMerge context' (FlitArg _ es (a :. inner             )) = handleMerge (addTypeToEnv a es context') inner
            handleMerge context' (PiArg   _ es (a :. (body, outputType))) = checkType' (addTypeToEnv a es context') body outputType 
            
    checkType' context (ReferenceVariableX _ _ m) shouldBe = case m `Map.lookup` modu of 
      Value v                -> checkType' context v shouldBe
      ValueAndAnnotation _ t -> checkAgainstKnownType t
      TypeConstructor    _ t -> checkAgainstKnownType t
      DataConstructor    _ t -> checkAgainstKnownType t
      where checkAgainstKnownType t = if isSubtype modu context t shouldBe then Right () else Left ()
    checkType' context (ParenthesizedExpressionX _ e) shouldBe = checkType' context e shouldBe
    -- Type checking rule for everything else
    --       context ⊢ e => t 
    -- ------------------------------------
    --       context ⊢ e <= t 
    -- Just infer a type and check that the type we inferred is the same as the one we want it to be
    checkType' context toCheck shouldBe = do
      toCheckType <- inferType modu context toCheck
      if isSubtype modu context toCheckType shouldBe then Right () else Left ()


combineUniverses :: ExpressionX Parsed i m -> ExpressionX Parsed i m -> Either () (ExpressionX Parsed i m)
combineUniverses f@(UniverseExpressionX _ i1) s@(UniverseExpressionX _ i2) = pure $ if i1 > i2 then f else s
combineUniverses _                          _                              = Left ()



