module CopyPropagatedProgramConverter where
import ClassyPrelude
import Basic
import Data.Maybe
import Ast
import qualified CopyPropagatedProgram as CPP
import qualified Data.Map.Justified as Map
import qualified Data.Map as Unjustified.Map
import Capability.Error

data Info = Info String
data Ty = TyVar Int
        | TyArr Ty Ty
        | TyAll String Ty
        | TySome String Ty
data Binding = NameBind
           | VarBind Ty
           | TyVarBind
data Term = TmVar Info Int Int
          | TmAbs Info String Ty Term
          | TmApp Info Term Term
          | TmTAbs Info String Term
          | TmTApp Info Term Ty
          | TmPack Info Ty Term Ty
          | TmUnpack Info String String Term Term





typeCheck = undefined



createMapOfIdentifiersToValues :: (HasThrow "perr" (ProductionError t p i) m, Ord i) => Module t p i -> (forall ph. Map.Map ph i (Expression t p (Map.Key ph i)) -> m out) -> m out
createMapOfIdentifiersToValues (Module _ declarations _) continuation = handleMapOutput mapOutput 
  where
    mapOutput = Map.withRecMap (Unjustified.Map.fromList declarationList) continuation
    handleMapOutput (Left missingReferences) = throw @"perr" . UndeclaredValuesReferenced $ fmap convertError missingReferences
      where convertError (k, f) = (k, fmap fst f)
    handleMapOutput (Right output) = output
    extractDec (Declaration (LowercaseIdentifier identifierName _) expression _) = (identifierName, expression)
    declarationList = fmap extractDec declarations 

getMainFunc :: (Ord k, IsString k, HasThrow "perr" (ProductionError t p i) f) => Module t p i -> Map.Map ph k a -> f a
getMainFunc moduleAST m = case "main" `Map.member` m of
  Nothing -> throw @"perr" . NoMain $ moduleAST
  Just i -> pure $ Map.lookup i m


--typeCheck :: (Ord k, IsString k, HasThrow "perr" (ProductionError t p i) f) => Map.Map ph k (Module u p i) -> m (Map.Map ph k (Module Metavariable p i))
{-typeCheck m = (flip mapM) m $ \case 
  FunctionLiteralExpression (FunctionLiteral t p i) t p -> pure TypechecksOkay
_ -> pure TypechecksOkay-}

evaluateMain :: (Ord k, Eq t, Eq p) => Map.Map ph k (Expression t p (Map.Key ph k)) -> Ast.Expression t p (Map.Key ph k) -> Ast.Expression t p (Map.Key ph k)
evaluateMain identValMap mainFunc =  fullyReduce mainFunc
  where
    reduceExpression e@(NumberLiteral _ _ _) = e
    reduceExpression (IdentifierExpression (LowercaseIdentifier ident _) _ _) = Map.lookup ident identValMap
    reduceExpression (FunctionLiteralExpression (FunctionLiteral boundIdentifiers subExpression _) _ _) = undefined
    reduceExpression e = e
    fullyReduce e = if reduceExpression e == e
                      then e
                      else fullyReduce (reduceExpression e)

example :: String
example = "module i `test module`\n\
          \test = -3.6\n\
          \myfunc = |a| -> a\n\
          \main = test\n\
          \"

-- runM (parseModule example >>= produceProgram)
produceProgram moduleAST = do
  createMapOfIdentifiersToValues moduleAST $ \valueMap -> do 
    mainFunc <- getMainFunc moduleAST valueMap
    let (NumberLiteral n _ _) = evaluateMain valueMap mainFunc
    pure n
