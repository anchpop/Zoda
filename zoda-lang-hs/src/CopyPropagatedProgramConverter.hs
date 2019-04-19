module CopyPropagatedProgramConverter where
import ClassyPrelude
import Basic
import Data.Maybe
import Ast
import qualified CopyPropagatedProgram as CPP
--import qualified Data.Map.Justified as Map
import qualified Data.Map as Map
import Capability.Error

createMapOfIdentifiersToValues :: (HasThrow "perr" (ProductionError i p) m, Ord i) => Module i p -> m (Map.Map i (Expression i p))
createMapOfIdentifiersToValues (Module header declarations _) = valueMap
 where
  valueMap = foldM addValueToValuemap (Map.fromList []) declarations
  addValueToValuemap m declaration@(Declaration (LowercaseIdentifier name _) expression _) =
    if isJust (Map.lookup name m) then throw @"perr" (ValueRedeclaration (declaration)) else pure (Map.insert name expression m)


checkNoUndefinedIdentifiers :: (HasThrow "perr" (ProductionError i p) m, Ord i) => (Map.Map i (Expression i p)) -> m (Map.Map i (Expression i p))
checkNoUndefinedIdentifiers m = mapM (checkIdentifiersInExpressionDefined m) (fmap snd . Map.toList $ m) *> pure m
 where
  getIdentifiersInExpression (NumberLiteralExpression _          _) = []
  getIdentifiersInExpression (IdentifierExpression    identifier _) = [identifier]
  checkIdentifierDefined m identifier@(LowercaseIdentifier s _) = isJust lookedUp where lookedUp = Map.lookup s m
  --checkIndentifiersDefined :: (Map.Map Text (Expression p)) -> [LowercaseIdentifier p] -> Either (ProductionError p) [LowercaseIdentifier p]
  checkIndentifiersDefined m identifiers = mapM getIdentifierDefinedOrError identifiers
    where getIdentifierDefinedOrError identifier = if checkIdentifierDefined m identifier then pure identifier else throw @"perr" (UndeclaredValueReferenced identifier)
  --checkIdentifiersInExpressionDefined :: (Map.Map Text (Expression p)) -> Expression p -> Either (ProductionError p) Bool
  checkIdentifiersInExpressionDefined m e = checkIndentifiersDefined m (getIdentifiersInExpression e) *> pure True


getMainFunc :: (HasThrow "perr" (ProductionError Text p) m) => Module Text p -> (Map.Map Text (Expression Text p)) -> m (Expression Text p)
getMainFunc moduleAST m = handleMainFunc mainFunc
  where
    mainFunc = Map.lookup "main" m
    handleMainFunc (Just f) = pure f
    handleMainFunc (Nothing) = throw @"perr" (NoMain moduleAST)



evaluateMain :: (Eq p, Ord i) => (Map.Map i (Expression i p)) -> Expression i p -> Expression i p
evaluateMain identValMap mainFunc = fullyReduce mainFunc
  where
    --reduceExpression :: Expression p -> Expression p
    reduceExpression e@(NumberLiteralExpression _ _) = e 
    reduceExpression (IdentifierExpression (LowercaseIdentifier ident _) _) = e
      where
        (Just e) = (Map.lookup ident identValMap)
    reduceExpression e@(FunctionLiteralExpression (FunctionLiteral _ _ _) _) = e
    fullyReduce e = if reduceExpression e == e 
                      then e
                      else fullyReduce (reduceExpression e)



example :: String
example = "module i `test module`\n\
          \test = -3.6\n\
          \main = test\n\
          \"


produceProgram moduleAST = do
  identValMap <- createMapOfIdentifiersToValues moduleAST
  checkNoUndefinedIdentifiers identValMap
  mainFunc <- getMainFunc moduleAST identValMap
  pure $ evaluateMain identValMap mainFunc
-- use with `runM (parseModule example >>= produceProgram)`