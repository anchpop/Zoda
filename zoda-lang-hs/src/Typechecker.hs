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


makeTelescope :: Env t p ph m i -> (NonEmpty (JustifiedExpression t p ph m i)) -> JustifiedExpression t p ph m i ->  JustifiedExpression t p ph m i
makeTelescope = undefined

findType :: (Nominal t, Nominal p, Nominal m, Nominal i) => Env t p ph m i -> JustifiedExpression t p ph m i -> JustifiedExpression t p ph m i
findType context (LambdaVariable (_, x) _ _) = 
  case x `lookup` context of
    Just t -> t 
    _      -> error "type error!"
findType context (FunctionApplicationExpression func args _ _) = 
  case findType context func of
    TArrowBinding scope _ _ -> doTelescope context args scope 
    _                       -> error "type error!"
findType context (FunctionLiteralExpression flit _ _) = undefined--makeTelescope context args bodyType
  --where bodyType = findType (context <> (NonEmpty.toList $ fmap (\((_, (a, _)), NoBind (Just v)) -> (a, v)) args)) body 
