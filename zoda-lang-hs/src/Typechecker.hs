module Typechecker where
import ClassyPrelude hiding (try, many)
import Nominal hiding ((.))
import Ast
import Basic
import Data.List.NonEmpty( NonEmpty( (:|) ) )
import qualified Data.List.NonEmpty as NonEmpty

type Env t p ph m i = [(Atom, JustifiedExpression t p ph m i)]

typecheck :: forall i p ph t m. JustifiedModule t p ph m i -> Either (ProductionError t p m i) ()
typecheck = undefined

sameValue :: Env t p ph m i -> JustifiedExpression t p ph m i -> JustifiedExpression t p ph m i -> Bool
sameValue = undefined


doTelescope :: Env t p ph m i ->  (Nominal t, Nominal p, Nominal m, Nominal i) => (NonEmpty (JustifiedExpression t p ph m i)) -> JustifiedTelescope t p ph m i ->  JustifiedExpression t p ph m i
doTelescope context (arg :| []) (Pi ((Just (_, (a, _)), NoBind typ) :. result)) = undefined
doTelescope context (arg :| []) (Pi ((Nothing         , NoBind typ) :. result)) = undefined


makeTelescope :: (Nominal t, Nominal p, Nominal m, Nominal i) => Env t p ph m i -> JustifiedFunctionLiteral t p ph m i -> Either () (JustifiedTelescope t p ph m i)
makeTelescope context (LastArg ((a, t) :. e)) = do
    typ <- inferType context e 
    pure $ Pi ((Just a, t) :. typ)

makeTelescope context (Arg (((i, (a, p)), NoBind t) :. scope)) = do
    scope' <- makeTelescope context' scope
    pure $ Scope ((Just (i, (a, p)), NoBind t) :. scope')  
  where context' = (a, t):context

mergFlitAndScope :: (Nominal t, Nominal p, Nominal m, Nominal i) => FunctionLiteral t p m i -> Telescope t p m i -> Either () (FunctionLiteral t p m i, Telescope t p m i)
mergFlitAndScope (Arg (((ia, (aa, pa)), ea) :. argsInner)) (Scope ((Just (is, (as, ps)), es) :. scopeInner)) = do 
  (mergedArgsInner, mergedScopeInner) <- mergFlitAndScope argsInner scopeInner
  let (a' :. (argsMerged, scopeMerged)) = merge (aa :. mergedArgsInner) (as :. mergedScopeInner)
  pure $ (Arg (((ia, (a', pa)), ea) :. argsMerged), Scope ((Just (is, (a', ps)), es):. scopeMerged))

mergFlitAndScope (Arg (((ia, (aa, pa)), ea) :. argsInner)) (Scope ((Nothing, es) :. scopeInner)) = do
  (mergedArgsInner, mergedScopeInner) <- mergFlitAndScope argsInner scopeInner
  pure $ (Arg (((ia, (aa, pa)), ea) :. mergedArgsInner), Scope ((Nothing, es) :. mergedScopeInner))

mergFlitAndScope (LastArg (((ia, (aa, pa)), ea) :. body)) (Pi ((Just (is, (as, ps)), es) :. outputType)) = do
  let (a' :. (bodyMerged, outputMerged)) = merge (aa :. body) (as :. outputType)
  pure $ (LastArg (((ia, (a', pa)), ea) :. bodyMerged), Pi ((Just (is, (a', ps)), es):. outputMerged))

mergFlitAndScope lastArg@(LastArg _) pi@(Pi ((Nothing, _) :. _)) = do
  pure $ (lastArg, pi)

mergFlitAndScope (LastArg _) (Scope _) = Left () 

mergFlitAndScope (Arg _) (Pi _) = Left ()

extendContextWithScope context scope = undefined -- adds all the (binding:type)s from the telescope to the context
checkScope context scope args = undefined -- takes a list of args and a telescope and checks thet all the args' types match the associated types of the telescope


inferType :: (Nominal t, Nominal p, Nominal m, Nominal i) => Env t p ph m i -> JustifiedExpression t p ph m i -> Either () (JustifiedExpression t p ph m i)
-- Typing rule for annotations
--       t is a type     context ⊢ e <= t
-- -----------------------------------------
--       context ⊢ (Annotation e t) => t
inferType context (Annotation expr typ t p) = checkType context expr typ *> pure typ -- TODO: also check that it's a valid type
-- Typing rule for variables
--            x : t ∈ context
-- -----------------------------------------
--           context ⊢ x => t
inferType context (LambdaVariable (_, x) _ _) = 
  case x `lookup` context of
    Just t -> pure t
    _      -> Left ()
-- Typing rule for function application
--  context ⊢ func => (Pi inT (x.outT))  context |- inT <= Type     context ⊢ in <= inT
-- --------------------------------------------------------------------------------------
--           context ⊢ (App func in) => (outT)    
inferType context (FunctionApplicationExpression func args t p) = undefined{-do
  funcType <- inferType context func 
  case funcType of 
    TArrowBinding scope _ _ -> do
                                checkScope context scope args
                                pure $ getOutputOfScope scope
    _                       -> Left ()-}
--inferType context (FunctionLiteralExpression flit t p) = do 
--  scope <- makeTelescope context flit
--  pure $ TArrowBinding scope t p
inferType context (AddExpression e1 e2 t p) = do 
  checkType context e1 (NatTypeExpression t p) 
  checkType context e2 (NatTypeExpression t p) 
  pure $ NatTypeExpression t p
inferType context (NumberLiteral _ t p) = pure $ NatTypeExpression t p
-- Typing rule for parentheses
--       context ⊢  x => t
-- ------------------------------
--       context ⊢ (x) => t
inferType context (ParenthesizedExpression e _ _) = inferType context e

checkType :: (Nominal t, Nominal p, Nominal m, Nominal i) => Env t p ph m i -> JustifiedExpression t p ph m i -> JustifiedExpression t p ph m i -> Either () ()
-- Type checking rule for lambda abstraction
--       context, x:t  ⊢  e <= s
-- ------------------------------------
--   context ⊢  Lam (x.e) <= Pi t (x.s)
-- note that the two `x`s in the binders have to actually be the "same" to correctly type it
-- since `x` might be mentioned in both `e` and `s`
-- the `merge` function in nominal is used for this
-- of couse, because we use telescopes instead of pi types it is a trickier
checkType context (FunctionLiteralExpression flit _ _) shouldBe = 
  case shouldBe of 
    TArrowBinding scope _ _ -> do (mergedFlit, mergedScope) <- mergFlitAndScope flit scope
                                  checkType (extendContextWithScope context mergedScope) (getBodyOfFlit mergedFlit) (getOutputOfScope mergedScope)
-- Type checking rule for everything else
--       context ⊢ e => t 
-- ------------------------------------
--       context ⊢ e <= t 
-- Just infer a type and check that the type we inferred is the same as the one we want it to be
checkType context toCheck shouldBe = do
  toCheckType <- inferType context toCheck
  if sameValue context toCheckType shouldBe then Right () else Left ()