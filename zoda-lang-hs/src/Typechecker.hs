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

-- |Takes some arguments and a function type
doTelescope :: Env t p ph m i ->  (Nominal t, Nominal p, Nominal m, Nominal i) => (NonEmpty (JustifiedExpression t p ph m i)) -> JustifiedTelescope t p ph m i ->  JustifiedExpression t p ph m i
doTelescope context (arg :| []) (Pi ((Just (_, (a, _)), NoBind typ) :. result)) = undefined
doTelescope context (arg :| []) (Pi ((Nothing         , NoBind typ) :. result)) = undefined


makeTelescope :: (Nominal t, Nominal p, Nominal m, Nominal i) => Env t p ph m i -> JustifiedFunctionLiteral t p ph m i -> JustifiedTelescope t p ph m i
makeTelescope context (LastArg ((a, t) :. e)) = Pi ((Just a, t) :. typ)
  where typ = inferType context e 
makeTelescope context (Arg (((i, (a, p)), NoBind t) :. scope)) = Scope ((Just (i, (a, p)), NoBind t) :. (makeTelescope context' scope))  
  where context' = (a, t):context

extendContextWithScope context scope = undefined -- adds all the (binding:type)s from the telescope to the context
checkScope context scope args = undefined -- takes a list of args and a telescope and checks thet all the args' types match the associated types of the telescope


inferType :: (Nominal t, Nominal p, Nominal m, Nominal i) => Env t p ph m i -> JustifiedExpression t p ph m i -> JustifiedExpression t p ph m i
inferType context (Annotation expr typ t p) = 
  (checkType context expr typ) `seq` typ -- also need to check that it's a valid type!!!
inferType context (FunctionLiteralExpression flit t p) = 
  TArrowBinding (makeTelescope context flit) t p
inferType context (FunctionApplicationExpression func args t p) = 
  case inferType context func of 
    TArrowBinding scope _ _ -> (checkScope context scope args) `seq` (getOutputOfScope scope) 
    _                       -> error "type error!"
inferType context (LambdaVariable (_, x) _ _) = 
  case x `lookup` context of
    Just t -> t
    _      -> error "value not in context, this is bad!"
inferType context (AddExpression e1 e2 t p) = 
  (checkType context e1 (NatTypeExpression t p)) `seq` (checkType context e2 (NatTypeExpression t p)) `seq` (NatTypeExpression t p)
inferType context (NumberLiteral _ t p) = NatTypeExpression t p
inferType context (ParenthesizedExpression e _ _) = inferType context e

checkType :: (Nominal t, Nominal p, Nominal m, Nominal i) => Env t p ph m i -> JustifiedExpression t p ph m i -> JustifiedExpression t p ph m i -> ()
checkType context (FunctionLiteralExpression flit _ _) shouldBe = 
  case shouldBe of 
    TArrowBinding scope _ _ -> checkType (extendContextWithScope context scope) (getBodyOfFlit flit) (getOutputOfScope scope)
    
checkType context (ParenthesizedExpression e _ _) shouldBe = checkType context e shouldBe
checkType context other shouldBe = if sameValue context t shouldBe then () else error "type error!" 
    where t = inferType context other