module Typechecker where
import ClassyPrelude hiding (try, many)
import Nominal hiding ((.))
import Ast
import Basic

type Env t p ph m i = [(Atom, JustifiedExpression t p ph m i)]

typecheck :: forall i p ph t m o. JustifiedModule t p ph m i -> Either (ProductionError t p m i) ()
typecheck modu = undefined

findType :: (Nominal t, Nominal p, Nominal m, Nominal i) => Env t p ph m i -> JustifiedExpression t p ph m i -> JustifiedExpression t p ph m i
findType context (LambdaVariable (_, x) _ _) = 
  case x `lookup` context of
    Just t -> t 
    _      -> error "type error!"
findType context (FunctionLiteralExpression (args :. func) _ _) = undefined
  