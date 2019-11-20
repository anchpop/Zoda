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

extendContextWithScope context scope = undefined -- adds all the (binding:type)s from the telescope to the context
checkScope context scope args = undefined -- takes a list of args and a telescope and checks thet all the args' types match the associated types of the telescope


inferType :: (Nominal t, Nominal p, Nominal m, Nominal i) => Env t p ph m i -> JustifiedExpression t p ph m i -> Either () (JustifiedExpression t p ph m i)
inferType context (Annotation expr typ t p) = checkType context expr typ *> pure typ -- also need to check that it's a valid type!!!
inferType context (FunctionLiteralExpression flit t p) = do 
  scope <- makeTelescope context flit
  pure $ TArrowBinding scope t p
inferType context (FunctionApplicationExpression func args t p) = do
  funcType <- inferType context func 
  case funcType of 
    TArrowBinding scope _ _ -> do
                                checkScope context scope args
                                pure $ getOutputOfScope scope
    _                       -> Left ()
inferType context (LambdaVariable (_, x) _ _) = 
  case x `lookup` context of
    Just t -> pure t
    _      -> Left ()
inferType context (AddExpression e1 e2 t p) = do 
  checkType context e1 (NatTypeExpression t p) 
  checkType context e2 (NatTypeExpression t p) 
  pure $ NatTypeExpression t p
inferType context (NumberLiteral _ t p) = pure $ NatTypeExpression t p
inferType context (ParenthesizedExpression e _ _) = inferType context e

checkType :: (Nominal t, Nominal p, Nominal m, Nominal i) => Env t p ph m i -> JustifiedExpression t p ph m i -> JustifiedExpression t p ph m i -> Either () ()
-- Typing rule for lambda abstraction
--       Gamma, x:t  |-  e : s
-- ------------------------------------
--   Gamma |-  Lam (x.e) : Pi t (x.s)
-- note that the two `x`s in the binders have to actually be the "same" to correctly type it
-- since `x` might be mentioned in both `e` and `s`
-- the `merge` function in nominal is used for this
checkType context (FunctionLiteralExpression flit _ _) shouldBe = 
  case shouldBe of 
    TArrowBinding scope _ _ -> let (megedScope, mergedFlit) = undefined  
                               in checkType (extendContextWithScope context scope) (getBodyOfFlit flit) (getOutputOfScope scope)
checkType context (ParenthesizedExpression e _ _) shouldBe = checkType context e shouldBe
checkType context toCheck shouldBe = do
  toCheckType <- inferType context toCheck
  if sameValue context toCheckType shouldBe then Right () else Left ()