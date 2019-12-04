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
addTypeToEnv a t (Env ts vs) = (Env ((a, t):ts) vs)
addValueToEnv a v (Env ts vs) = (Env ts ((a, v):vs))
lookupTypeInEnv a (Env ts _) = a `lookup` ts
getValueMap (Env _ vs) = vs
instance Semigroup (Env t p ph m i) where
  (Env t1 v1) <> (Env t2 v2) = Env (t1 <> t2) (v1 <> v2)
instance Monoid (Env t p ph m i) where
  mempty = Env [] [] 

typecheck modu = case traverse (inferType modu mempty) modu of
  Left () -> Left TypeErr
  Right v -> Right ()

sameValue :: Constraints t p m i => JustifiedModule t p ph m i -> Env t p ph m i -> JustifiedExpression t p ph m i -> JustifiedExpression t p ph m i -> JustifiedExpression t p ph m i -> JustifiedExpression t p ph m i -> Bool
sameValue modu context e1 t1 e2 t2 = e1Normalized == e2Normalized
  where e1Normalized = normalize modu (getValueMap context) e1 t1
        e2Normalized = normalize modu (getValueMap context) e2 t2


{-
makeTelescope :: (Bindable t, Bindable p, Bindable m, Bindable i, Eq t, Eq p, Eq m, Eq i, Ord m) => JustifiedModule t p ph m i -> Env t p ph m i -> JustifiedFunctionLiteral t p ph m i -> Either () (JustifiedTelescope t p ph m i)
makeTelescope modu context (LastArg ((a, t) :. e)) = do
    typ <- inferType context e 
    pure $ Pi ((Just a, t) :. typ)
makeTelescope modu context (Arg (((i, (a, p)), NoBind t) :. scope)) = do
    scope' <- makeTelescope modu context' scope
    pure $ Scope ((Just (i, (a, p)), NoBind t) :. scope')  
  where context' = addTypeToEnv a t context
-}

 

data MergedFlitAndScope t p m i = FlitArg (Expression t p m i) (Expression t p m i) (Bind Atom (MergedFlitAndScope t p m i)) 
                                | PiArg   (Expression t p m i) (Expression t p m i) (Bind Atom (Expression t p m i, Expression t p m i)) 
                                deriving (Show, Eq, Typeable, NominalSupport, NominalShow, Generic, Nominal)

type Constraints t p m i = (Bindable t, Bindable p, Bindable m, Bindable i, Eq t, Eq p, Eq m, Eq i, Ord m, Show t, NominalShow t, Show p, NominalShow p, Show m, NominalShow m, Show i, NominalShow i)

mergedFlitAndScope :: Constraints t p m i => FunctionLiteral t p m i -> Telescope t p m i -> Either () (MergedFlitAndScope t p m i)
mergedFlitAndScope (Arg ea ((aa, ia, pa) :. argsInner)) (Scope es (Just (as, is, ps) :. scopeInner)) = do 
  let (a' :. (argsMergedInner, scopeMergedInner)) = merge (aa :. argsInner) (as :. scopeInner)
  mergedInner <- mergedFlitAndScope argsInner scopeInner
  pure $ FlitArg ea es (a'  :. mergedInner)

mergedFlitAndScope (Arg ea ((aa, ia, pa) :. argsInner)) (Scope es (Nothing :. scopeInner)) = do
  mergedInner <- mergedFlitAndScope argsInner scopeInner
  pure $ FlitArg ea es (aa :. mergedInner)

mergedFlitAndScope (LastArg ea ((aa, ia, pa) :. body)) (Pi es ((Just (as, is, ps)) :. outputType)) = do
  let (a' :. (bodyMerged, outputTypedMerged)) = merge (aa :. body) (as :. outputType)
  pure $ PiArg ea es (a' :. (bodyMerged, outputTypedMerged))

mergedFlitAndScope lastArg@(LastArg ea ((aa, ia, pa) :. body)) pi@(Pi es (Nothing :. outputType)) = do
  pure $ PiArg ea es (aa :. (body, outputType))

mergedFlitAndScope (LastArg _ _) (Scope _ _) = Left () 

mergedFlitAndScope (Arg _ _) (Pi _ _) = Left ()

-- |Takes a context, a list of args, and a telescope.
-- It checks that the input arguments' types match the types specified in 
-- the telescope. It then returns the output type with the appropriate substitutions done.
-- if you pass `Int, 3` to `(a: Type, v: a) -> a` it should return `Int`
checkScopeAndGetOutput modu context (arg NonEmpty.:| []) (Pi argType ((Just (a, _, _)) :. bodyType)) = do 
  checkType modu context arg argType
  pure $ substExpr a arg bodyType
checkScopeAndGetOutput modu context (arg NonEmpty.:| []) (Pi argType (Nothing :. bodyType)) = do 
  checkType modu context arg argType
  pure bodyType 
checkScopeAndGetOutput modu context (arg NonEmpty.:| (r':rest)) (Scope argType ((Just (a, _, _)) :. innerScope)) = do 
  checkType modu context arg argType
  checkScopeAndGetOutput modu context (r' NonEmpty.:| rest) (substTelescope a arg innerScope)
checkScopeAndGetOutput modu context (arg NonEmpty.:| []) (Scope _ _) = Left ()
checkScopeAndGetOutput modu context (arg NonEmpty.:| _) (Pi _ _) = Left () 
  
extendContextWithScope context (Pi e1 ((Just (a, i, p)) :. e2)) = addTypeToEnv a e1 context
extendContextWithScope context (Pi _ (Nothing :. e2)) = context
extendContextWithScope context (Scope e ((Just (a, i, p)) :. scope)) = addTypeToEnv a e (extendContextWithScope context scope)
extendContextWithScope context (Scope _ (Nothing :. scope)) = extendContextWithScope context scope


inferType :: Constraints t p m i => JustifiedModule t p ph m i -> Env t p ph m i -> JustifiedExpression t p ph m i -> Either () (JustifiedExpression t p ph m i)
inferType modu = inferType' where
  -- Typing rule for universes
  --     ---------------------------------------
  --           Universe i => Universe (i + 1)
  inferType' context (UniverseExpression i t p) = pure $ UniverseExpression (i + 1) t p
  -- Typing rule for Nat
  --     ---------------------------------------
  --           Nat => Universe 1
  inferType' context (NatTypeExpression t p) = pure $ UniverseExpression 1 t p
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
    where makeTelescope context' (Arg     e ((a, i, p) :. rest)) = do rest' <- makeTelescope (addTypeToEnv a e context') rest
                                                                      pure $ Scope e (Just (a, i, p) :. rest')  
          makeTelescope context' (LastArg e ((a, i, p) :. body)) = do bodyType <- inferType' (addTypeToEnv a e context') body
                                                                      pure $ Pi e (Just (a, i, p) :. bodyType)
  --         context ⊢ x1 <= Nat   context ⊢ x2 <= Nat
  --     -------------------------------------------------------
  --             context ⊢ x1 + x2 => Nat
  inferType' context (AddExpression e1 e2 t p) = do 
    checkType modu context e1 (NatTypeExpression t p) 
    checkType modu context e2 (NatTypeExpression t p) 
    pure $ NatTypeExpression t p
  --     ---------------------------------------------------------
  --             context ⊢ (1 | 2 | 3 | ...) => Nat
  inferType' context (NumberLiteral _ t p) = pure $ NatTypeExpression t p
  -- Typing rule for parentheses
  --          context ⊢  x => t
  --     -----------------------------
  --          context ⊢ (x) => t
  inferType' context (ParenthesizedExpression e _ _) = inferType' context e
  inferType' context (TSigmaBinding e1 ((a, _, _) :. e2) _ _) = do
    e1T <- inferType' context e1
    e2T <- inferType' (addTypeToEnv a e1 context) e2
    pure $ combineUniverses e1T e2T
  inferType' context (TSigmaBinding e1 ((a, _, _) :. e2) _ _) = do
    e1T <- inferType' context e1
    e2T <- inferType' (addTypeToEnv a e1 context) e2
    pure $ combineUniverses e1T e2T
  inferType' context (ReferenceVariable i m _ _) = inferType' context (m `Map.lookup` modu)
  inferType' context (FirstExpression e _ _) = do 
    eT <- inferType' context e 
    case eT of 
      TSigmaBinding e _ _ _ -> pure e 
      _                     -> Left ()
  inferType' context (SecondExpression _ _ _) = undefined
  inferType' context (PairExpression _ _ _ _) = undefined
  inferType' context (TArrowBinding _ _ _) = undefined
  inferType modu context other                 = error $ "couldn't infer a type for " <> show other 

checkType :: Constraints t p m i => JustifiedModule t p ph m i -> Env t p ph m i -> JustifiedExpression t p ph m i -> JustifiedExpression t p ph m i -> Either () ()
-- Type checking rule for lambda abstraction
--          context, x:t ⊢ e <= s
--     ------------------------------------
--       context ⊢  Lam t (x.e) <= Pi t (x.s)
-- note that the two `x`s in the binders have to actually be the "same" to correctly type it
-- since `x` might be mentioned in both `e` and `s`
-- the `merge` function in nominal is used for this
-- of couse, because we use telescopes instead of pi types it is a trickier
checkType modu context (FunctionLiteralExpression flit _ _) shouldBe = 
  case shouldBe of 
    TArrowBinding scope _ _ -> do mergedFlitScope <- mergedFlitAndScope flit scope
                                  handleMerge context mergedFlitScope--checkType modu (extendContextWithScope context mergedScope) (getBodyOfFlit mergedFlit) (getOutputOfScope mergedScope)
  where handleMerge context' (FlitArg _ es (a :. inner             )) = handleMerge (addTypeToEnv a es context') inner
        handleMerge context' (PiArg   _ es (a :. (body, outputType))) = checkType modu (addTypeToEnv a es context') body outputType 
        
checkType modu context (ReferenceVariable i m _ _) shouldBe = checkType modu context (m `Map.lookup` modu) shouldBe
checkType modu context (ParenthesizedExpression e _ _) shouldBe = checkType modu context e shouldBe
-- Type checking rule for everything else
--       context ⊢ e => t 
-- ------------------------------------
--       context ⊢ e <= t 
-- Just infer a type and check that the type we inferred is the same as the one we want it to be
checkType modu context toCheck shouldBe = do
  toCheckType <- inferType modu context toCheck
  if sameValue modu context toCheckType (UniverseExpression 1 undefined undefined) shouldBe (UniverseExpression 1 undefined undefined) then Right () else Left ()

{-
getUniverse modu = getUniverse'
  where getUniverse' (ParenthesizedExpression e _ _) = getUniverse' e 
        getUniverse' (FirstExpression (PairExpression e _ _ _) _ _) = getUniverse' e
        getUniverse' (SecondExpression (PairExpression _ e _ _) _ _) = getUniverse' e
        getUniverse' (TSigmaBinding e1 (_ .: e2) _ _) = getUniverse' e1 <> getUniverse' e2
        getUniverse' (UniverseExpression i _ _) = Just $ Semigroup.Max (i+1)
        getUniverse' (NumberLiteral i _ _) = Nothing
        getUniverse' (AddExpression _ _ _ _) = Nothing
        getUniverse' (ReferenceVariable i m _ _) = m `Map.lookup` modu
        getUniverse' (ReferenceVariable i m _ _) = m `Map.lookup` modu
-}
combineUniverses f@(UniverseExpression i1 _ _)  s@(UniverseExpression i2 _ _) = if i1 > i2 then f else s 