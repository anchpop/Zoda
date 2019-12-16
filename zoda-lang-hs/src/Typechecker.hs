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


data Env t p ph m i = Env {types :: [(Atom, JustifiedExpression t p ph m i)], values :: [(Atom, JustifiedExpression t p ph m i)]} deriving (Eq, Show)
addTypeToEnv :: Atom -> JustifiedExpression t p ph m i -> Env t p ph m i -> Env t p ph m i
addTypeToEnv a t (Env ts vs) = (Env ((a, t):ts) vs)
addValueToEnv :: Atom -> JustifiedExpression t p ph m i -> Env t p ph m i -> Env t p ph m i
addValueToEnv a v (Env ts vs) = (Env ts ((a, v):vs))
lookupTypeInEnv :: Atom -> Env t p ph m i -> Maybe (Expression t p (Map.Key ph m) i)
lookupTypeInEnv a (Env ts _) = a `lookup` ts
getValueMap :: Env t p ph m i -> [(Atom, JustifiedExpression t p ph m i)]
getValueMap (Env _ vs) = vs
instance Semigroup (Env t p ph m i) where
  (Env t1 v1) <> (Env t2 v2) = Env (t1 <> t2) (v1 <> v2)
instance Monoid (Env t p ph m i) where
  mempty = Env [] [] 
typecheck :: (Bindable t1, Bindable p1, Bindable m1, Bindable i1, Eq t1, Eq p1, Eq i1, Ord m1, Show t1, Show p1, Show m1, Show i1, NominalShow t1, NominalShow p1, NominalShow m1, NominalShow i1) => JustifiedModule t1 p1 ph m1 i1 -> Either (ProductionError t2 p2 m2 i2) ()
typecheck modu = case traverse typecheckDelcaration modu of
  Left () -> Left TypeErr
  Right _ -> Right ()
  where typecheckDelcaration (Value v)         = inferType modu mempty v *> pure ()
        typecheckDelcaration (ValueAndAnnotation v t) = checkType modu mempty v t
        typecheckDelcaration (Constructor _) = pure ()

isSubtype :: forall t p m i ph. Constraints t p m i => JustifiedModule t p ph m i -> Env t p ph m i -> JustifiedExpression t p ph m i -> JustifiedExpression t p ph m i -> Bool
isSubtype modu context e1 e2 = case (e1Normalized, e2Normalized) of 
    (UniverseExpression uni1 _ _, UniverseExpression uni2 _ _) -> uni1 <= uni2
    -- probably should do something with functions and pairs here
    -- but not going to stress it too much since it might change soon
    _                                                          -> e1Normalized == e2Normalized
  where e1Normalized = normalizeType modu (getValueMap context) e1 
        e2Normalized = normalizeType modu (getValueMap context) e2 



data MergedFlitAndScope t p m i = FlitArg (Expression t p m i) (Expression t p m i) (Bind Atom (MergedFlitAndScope t p m i)) 
                                | PiArg   (Expression t p m i) (Expression t p m i) (Bind Atom (Expression t p m i, Expression t p m i)) 
                                deriving (Show, Eq, Typeable, NominalSupport, NominalShow, Generic, Nominal)

type Constraints t p m i = (Bindable t, Bindable p, Bindable m, Bindable i, Eq t, Eq p, Eq m, Eq i, Ord m, Show t, NominalShow t, Show p, NominalShow p, Show m, NominalShow m, Show i, NominalShow i)

mergedFlitAndScope :: forall t p m i. Constraints t p m i => FunctionLiteral t p m i -> Telescope t p m i -> Either () (MergedFlitAndScope t p m i)
mergedFlitAndScope (Arg ea ((aa, _, _) :. argsInner)) (Scope es (Just (as, _, _) :. scopeInner)) = do 
  let (a' :. (argsMergedInner, scopeMergedInner)) = merge (aa :. argsInner) (as :. scopeInner)
  mergedInner <- mergedFlitAndScope argsMergedInner scopeMergedInner
  pure $ FlitArg ea es (a'  :. mergedInner)

mergedFlitAndScope (Arg ea ((aa, _, _) :. argsInner)) (Scope es (Nothing :. scopeInner)) = do
  mergedInner <- mergedFlitAndScope argsInner scopeInner
  pure $ FlitArg ea es (aa :. mergedInner)

mergedFlitAndScope (LastArg ea ((aa, _, _) :. body)) (Pi es ((Just (as, _, _)) :. outputType)) = do
  let (a' :. (bodyMerged, outputTypedMerged)) = merge (aa :. body) (as :. outputType)
  pure $ PiArg ea es (a' :. (bodyMerged, outputTypedMerged))

mergedFlitAndScope (LastArg ea ((aa, _, _) :. body)) (Pi es (Nothing :. outputType)) = do
  pure $ PiArg ea es (aa :. (body, outputType))

mergedFlitAndScope (LastArg _ _) (Scope _ _) = Left () 

mergedFlitAndScope (Arg _ _) (Pi _ _) = Left ()

-- |Takes a context, a list of args, and a telescope.
-- It checks that the input arguments' types match the types specified in 
-- the telescope. It then returns the output type with the appropriate substitutions done.
-- if you pass `Int, 3` to `(a: Type, v: a) -> a` it should return `Int`
checkScopeAndGetOutput :: forall t p m i ph. (Bindable t, Bindable p, Bindable m, Bindable i, Eq t, Eq p, Eq i, Ord m, Show t, Show p, Show m, Show i, NominalShow t, NominalShow p, NominalShow m, NominalShow i) => JustifiedModule t p ph m i -> Env t p ph m i -> NonEmpty (JustifiedExpression t p ph m i) -> Telescope t p (Map.Key ph m) i -> Either () (Expression t p (Map.Key ph m) i)
checkScopeAndGetOutput modu = checkScopeAndGetOutput'
  where
    checkScopeAndGetOutput' context (arg NonEmpty.:| []) (Pi argType ((Just (a, _, _)) :. bodyType)) = do 
      checkType modu context arg argType
      pure $ substExpr a arg bodyType
    checkScopeAndGetOutput' context (arg NonEmpty.:| []) (Pi argType (Nothing :. bodyType)) = do 
      checkType modu context arg argType
      pure bodyType 
    checkScopeAndGetOutput' context (arg NonEmpty.:| (r':rest)) (Scope argType ((Just (a, _, _)) :. innerScope)) = do 
      checkType modu context arg argType
      checkScopeAndGetOutput' context (r' NonEmpty.:| rest) (substTelescope a arg innerScope)
    checkScopeAndGetOutput' context (arg NonEmpty.:| (r':rest)) (Scope argType (Nothing :. innerScope)) = do 
      checkType modu context arg argType
      checkScopeAndGetOutput' context (r' NonEmpty.:| rest) innerScope
    checkScopeAndGetOutput' _ (_ NonEmpty.:| []) (Scope _ _) = Left ()
    checkScopeAndGetOutput' _ (_ NonEmpty.:| _) (Pi _ _) = Left () 

inferType :: Constraints t p m i => JustifiedModule t p ph m i -> Env t p ph m i -> JustifiedExpression t p ph m i -> Either () (JustifiedExpression t p ph m i)
inferType modu = inferType' where
  -- Typing rule for universes
  --     ---------------------------------------
  --           Universe i => Universe (i + 1)
  inferType' _ (UniverseExpression i t p) = pure $ UniverseExpression (i + 1) t p
  -- Typing rule for Nat
  --     ---------------------------------------
  --           Nat => Universe 1
  inferType' _ (NatTypeExpression t p) = pure $ UniverseExpression 1 t p
  -- Typing rule for annotations
  --        t is a type      context ⊢ e <= t
  --     ---------------------------------------
  --         context ⊢ (Annotation e t) => t
  inferType' context (Annotation expr typ _ _) = checkType modu context expr typ *> pure typ -- TODO: also check that it's a valid type
  -- Typing rule for variables
  --            x : t ∈ context
  --     -----------------------------------------
  --           context ⊢ x => t
  inferType' context (LambdaVariable (x, _) _ _) = 
    case x `lookupTypeInEnv` context of
      Just t -> pure t
      _      -> Left ()
  -- Typing rule for function application
  --      context ⊢ func => (Pi inT (x.outT))  context |- inT <= Type     context ⊢ in <= inT
  --     --------------------------------------------------------------------------------------
  --               context ⊢ (App func in) => (outT[in/x]) 
  inferType' context (FunctionApplicationExpression func args _ _) = do
    funcType <- inferType' context func 
    case funcType of 
      TArrowBinding scope _ _ -> checkScopeAndGetOutput modu context args scope
      _                       -> Left ()
  -- Typing rule for functions
  --               context, in : inT ⊢ body => bodyT
  --     ---------------------------------------------------------
  --             context ⊢ (in : inT).body => inT -> bodyT
  inferType' context (FunctionLiteralExpression flit t p) = do 
    scope <- makeTelescope context flit
    pure $ TArrowBinding scope t p
    where makeTelescope context' (Arg     e ((a, i, p1) :. rest)) = do rest' <- makeTelescope (addTypeToEnv a e context') rest
                                                                       pure $ Scope e (Just (a, i, p1) :. rest')  
          makeTelescope context' (LastArg e ((a, i, p1) :. body)) = do bodyType <- inferType' (addTypeToEnv a e context') body
                                                                       pure $ Pi e (Just (a, i, p1) :. bodyType)
  --         context ⊢ x1 <= Nat   context ⊢ x2 <= Nat
  --     -------------------------------------------------------
  --             context ⊢ x1 + x2 => Nat
  inferType' context (AddExpression e1 e2 t p) = do 
    checkType modu context e1 (NatTypeExpression t p) 
    checkType modu context e2 (NatTypeExpression t p) 
    pure $ NatTypeExpression t p
  --     ---------------------------------------------------------
  --             context ⊢ (1 | 2 | 3 | ...) => Nat
  inferType' _ (NumberLiteral _ t p) = pure $ NatTypeExpression t p
  -- Typing rule for parentheses
  --          context ⊢  x => t
  --     -----------------------------
  --          context ⊢ (x) => t
  inferType' context (ParenthesizedExpression e _ _) = inferType' context e
  inferType' context (TSigmaBinding e1 (Just (a, _, _) :. e2) _ _) = do
    e1T <- inferType' context e1
    e2T <- inferType' (addTypeToEnv a e1 context) e2
    combineUniverses e1T e2T
  inferType' context (TSigmaBinding e1 (Nothing :. e2) _ _) = do
    e1T <- inferType' context e1
    e2T <- inferType' context e2
    combineUniverses e1T e2T
  inferType' context (ReferenceVariable _ m _ _) = case m `Map.lookup` modu of 
    Value v                -> inferType' context v 
    ValueAndAnnotation _ t -> pure t 
    Constructor t          -> pure t 
  inferType' context (FirstExpression e _ _) = do 
    eT <- inferType' context e 
    case eT of 
      TSigmaBinding fstT _ _ _ -> pure fstT
      _                        -> Left ()
  inferType' context (SecondExpression e _ _) = do
    eT <- inferType' context e 
    case eT of 
      TSigmaBinding e1 (Just (a, _, _) :. e2) _ _ -> pure (substExpr a e1 e2) 
      TSigmaBinding _  (Nothing        :. e2) _ _ -> pure e2
      _                                           -> Left ()
  inferType' context (PairExpression e1T e2T t p) = do
    e1TT <- inferType' context e1T 
    e2TT <- inferType' context e2T 
    pure $ TSigmaBinding e1TT (Nothing :. e2TT) t p
  inferType' context (TArrowBinding flit _ _) = combineArrow flit
    where 
      combineArrow (Scope argT (_ :. rest)) = do 
        argTT <- inferType' context argT
        restT <- combineArrow rest
        combineUniverses argTT restT
      combineArrow (Pi argT (_ :. outT)) = do 
        argTT <- inferType' context argT
        outTT <- inferType' context outT
        combineUniverses argTT outTT
  inferType' _ other = error $ "couldn't infer a type for " <> show other 

checkType :: Constraints t p m i => JustifiedModule t p ph m i -> Env t p ph m i -> JustifiedExpression t p ph m i -> JustifiedExpression t p ph m i -> Either () ()
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
    checkType' context (FunctionLiteralExpression flit _ _) shouldBe = 
      case shouldBe of 
        TArrowBinding scope _ _ -> do mergedFlitScope <- mergedFlitAndScope flit scope
                                      handleMerge context mergedFlitScope
        _                       -> Left ()
      where handleMerge context' (FlitArg _ es (a :. inner             )) = handleMerge (addTypeToEnv a es context') inner
            handleMerge context' (PiArg   _ es (a :. (body, outputType))) = checkType' (addTypeToEnv a es context') body outputType 
            
    checkType' context (ReferenceVariable _ m _ _) shouldBe = case m `Map.lookup` modu of 
      Value v                 -> checkType' context v shouldBe
      ValueAndAnnotation _ t -> if isSubtype modu context t shouldBe then Right () else Left ()
      Constructor           t -> if isSubtype modu context t shouldBe then Right () else Left ()
    checkType' context (ParenthesizedExpression e _ _) shouldBe = checkType' context e shouldBe
    -- Type checking rule for everything else
    --       context ⊢ e => t 
    -- ------------------------------------
    --       context ⊢ e <= t 
    -- Just infer a type and check that the type we inferred is the same as the one we want it to be
    checkType' context toCheck shouldBe = do
      toCheckType <- inferType modu context toCheck
      if isSubtype modu context toCheckType shouldBe then Right () else Left ()


combineUniverses :: Expression t p m i -> Expression t p m i -> Either () (Expression t p m i)
combineUniverses f@(UniverseExpression i1 _ _) s@(UniverseExpression i2 _ _) = pure $ if i1 > i2 then f else s
combineUniverses _                           _                                   = Left ()



