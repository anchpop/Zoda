module Typechecker where
import ClassyPrelude hiding (try, many)
import Nominal hiding ((.))
import Ast
import Basic
import Data.List.NonEmpty( NonEmpty( (:|) ) )
import Interpreter
import qualified Data.List.NonEmpty as NonEmpty

data Env t p ph m i = Env {types :: [(Atom, JustifiedExpression t p ph m i)], values :: [(Atom, JustifiedExpression t p ph m i)]}
addTypeToEnv a t (Env ts vs) = (Env ((a, t):ts) vs)
addValueToEnv a v (Env ts vs) = (Env ts ((a, v):vs))
lookupTypeInEnv a (Env ts _) = a `lookup` ts
getValueMap (Env _ vs) = vs

typecheck :: forall i p ph t m. JustifiedModule t p ph m i -> Either (ProductionError t p m i) ()
typecheck = undefined

sameValue :: (Bindable t, Bindable p, Bindable m, Bindable i, Eq t, Eq p, Eq m, Eq i, Ord m) => JustifiedModule t p ph m i -> Env t p ph m i -> JustifiedExpression t p ph m i -> JustifiedExpression t p ph m i -> JustifiedExpression t p ph m i -> JustifiedExpression t p ph m i -> Bool
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

mergFlitAndScope :: (Bindable t, Bindable p, Bindable m, Bindable i, Eq t, Eq p, Eq m, Eq i, Ord m) => FunctionLiteral t p m i -> Telescope t p m i -> Either () (FunctionLiteral t p m i, Telescope t p m i)
mergFlitAndScope (Arg ea ((aa, ia, pa) :. argsInner)) (Scope es (Just (as, is, ps) :. scopeInner)) = do 
  (mergedArgsInner, mergedScopeInner) <- mergFlitAndScope argsInner scopeInner
  let (a' :. (argsMerged, scopeMerged)) = merge (aa :. mergedArgsInner) (as :. mergedScopeInner)
  pure $ (Arg ea (((a', ia, pa)) :. argsMerged), Scope es ((Just (a', is, ps)) :. scopeMerged))

mergFlitAndScope (Arg ea ((aa, ia, pa) :. argsInner)) (Scope es (Nothing :. scopeInner)) = do
  (mergedArgsInner, mergedScopeInner) <- mergFlitAndScope argsInner scopeInner
  pure $ (Arg ea (((aa, ia, pa)) :. mergedArgsInner), Scope es (Nothing :. mergedScopeInner))

mergFlitAndScope (LastArg ea ((aa, ia, pa) :. body)) (Pi es ((Just (as, is, ps)) :. outputType)) = do
  let (a' :. (bodyMerged, outputMerged)) = merge (aa :. body) (as :. outputType)
  pure $ (LastArg ea (((a', ia,  pa)) :. bodyMerged), Pi es ((Just (a', is, ps)) :. outputMerged))

mergFlitAndScope lastArg@(LastArg _ _) pi@(Pi _ (Nothing :. _)) = do
  pure $ (lastArg, pi)

mergFlitAndScope (LastArg _ _) (Scope _ _) = Left () 

mergFlitAndScope (Arg _ _) (Pi _ _) = Left ()

-- |Takes a context, a list of args, and a telescope.
-- It checks that the input arguments' types match the types specified in 
-- the telescope. It then returns the normalized output type (normalized because
-- we may need to substitute some values in the type with the arguments, i.e.
-- if you pass `Int, 3` to `(a: Type, v: a) -> a` we actually need to do mulitple normalizations)
checkScopeAndGetOutput context  = undefined--(LastArg (((), ) :. )) (Pi ((Just (), ) :. )) = 

extendContextWithScope context (Pi e1 ((Just (a, i, p)) :. e2)) = addTypeToEnv a e1 context
extendContextWithScope context (Pi _ (Nothing :. e2)) = context
extendContextWithScope context (Scope e ((Just (a, i, p)) :. scope)) = addTypeToEnv a e (extendContextWithScope context scope)
extendContextWithScope context (Scope _ (Nothing :. scope)) = extendContextWithScope context scope


inferType :: (Bindable t, Bindable p, Bindable m, Bindable i, Eq t, Eq p, Eq m, Eq i, Ord m) => JustifiedModule t p ph m i -> Env t p ph m i -> JustifiedExpression t p ph m i -> Either () (JustifiedExpression t p ph m i)
-- Typing rule for annotations
--        t is a type      context ⊢ e <= t
--     ---------------------------------------
--         context ⊢ (Annotation e t) => t
inferType modu context (Annotation expr typ t p) = checkType modu context expr typ *> pure typ -- TODO: also check that it's a valid type
-- Typing rule for variables
--            x : t ∈ context
--     -----------------------------------------
--           context ⊢ x => t
inferType modu context (LambdaVariable (x, _) _ _) = 
  case x `lookupTypeInEnv` context of
    Just t -> pure t
    _      -> Left ()
-- Typing rule for function application
--      context ⊢ func => (Pi inT (x.outT))  context |- inT <= Type     context ⊢ in <= inT
--     --------------------------------------------------------------------------------------
--               context ⊢ (App func in) => (outT[in/x]) 
inferType modu context (FunctionApplicationExpression func args t p) = do
  funcType <- inferType modu context func 
  case funcType of 
    TArrowBinding scope _ _ -> checkScopeAndGetOutput context args scope
    _                       -> Left ()
--inferType modu context (FunctionLiteralExpression flit t p) = do 
--  scope <- makeTelescope context flit
--  pure $ TArrowBinding scope t p
inferType modu context (AddExpression e1 e2 t p) = do 
  checkType modu context e1 (NatTypeExpression t p) 
  checkType modu context e2 (NatTypeExpression t p) 
  pure $ NatTypeExpression t p
inferType modu context (NumberLiteral _ t p) = pure $ NatTypeExpression t p
-- Typing rule for parentheses
--          context ⊢  x => t
--     -----------------------------
--          context ⊢ (x) => t
inferType modu context (ParenthesizedExpression e _ _) = inferType modu context e

checkType :: (Bindable t, Bindable p, Bindable m, Bindable i, Eq t, Eq p, Eq m, Eq i, Ord m) => JustifiedModule t p ph m i ->  Env t p ph m i -> JustifiedExpression t p ph m i -> JustifiedExpression t p ph m i -> Either () ()
-- Type checking rule for lambda abstraction
--          context, x:t  ⊢  e <= s
--     ------------------------------------
--       context ⊢  Lam (x.e) <= Pi t (x.s)
-- note that the two `x`s in the binders have to actually be the "same" to correctly type it
-- since `x` might be mentioned in both `e` and `s`
-- the `merge` function in nominal is used for this
-- of couse, because we use telescopes instead of pi types it is a trickier
checkType modu context (FunctionLiteralExpression flit _ _) shouldBe = 
  case shouldBe of 
    TArrowBinding scope _ _ -> do (mergedFlit, mergedScope) <- mergFlitAndScope flit scope
                                  checkType modu (extendContextWithScope context mergedScope) (getBodyOfFlit mergedFlit) (getOutputOfScope mergedScope)
-- Type checking rule for everything else
--       context ⊢ e => t 
-- ------------------------------------
--       context ⊢ e <= t 
-- Just infer a type and check that the type we inferred is the same as the one we want it to be
checkType modu context toCheck shouldBe = do
  toCheckType <- inferType modu context toCheck
  if sameValue modu context toCheckType (UniverseExpression 1 undefined undefined) shouldBe (UniverseExpression 1 undefined undefined) then Right () else Left ()