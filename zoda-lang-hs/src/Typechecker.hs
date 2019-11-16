module Typechecker where
import ClassyPrelude hiding (try, many)
import Nominal hiding ((.))
import Ast
import Basic
import qualified Data.List.NonEmpty as NonEmpty

type Env t p ph m i = [(Atom, JustifiedExpression t p ph m i)]

typecheck :: forall i p ph t m o. JustifiedModule t p ph m i -> Either (ProductionError t p m i) ()
typecheck modu = undefined

sameType = undefined
doTelescope = undefined
makeTelescope = undefined

findType :: (Nominal t, Nominal p, Nominal m, Nominal i) => Env t p ph m i -> JustifiedExpression t p ph m i -> JustifiedExpression t p ph m i
findType context (LambdaVariable (_, x) _ _) = 
  case x `lookup` context of
    Just t -> t 
    _      -> error "type error!"
findType context (FunctionApplicationExpression func args _ _) = 
  case findType context func of
    TArrowBinding scope _ _ -> doTelescope args scope 
    _                       -> error "type error!"
findType context (FunctionLiteralExpression (args :. body) _ _) = makeTelescope args bodyType
  where bodyType = findType (context <> (NonEmpty.toList $ fmap (\((_, (a, _)), NoBind (Just v)) -> (a, v)) args)) body 
