module CopyPropagatedProgramConverter where
import ClassyPrelude
import Basic
import Data.Maybe
import Ast
import qualified CopyPropagatedProgram as CPP
import qualified Data.Map as Map
import Capability.Error

createMapOfIdentifiersToValues :: (HasThrow "perr" (ProductionError p) m) => Module p -> m (Map.Map Text (Expression p))
createMapOfIdentifiersToValues (Module header declarations _) = valueMap
 where
  valueMap = foldM addValueToValuemap Map.empty declarations
  addValueToValuemap m declaration@(Declaration (LowercaseIdentifier name _) expression _) =
    if isJust (Map.lookup name m) then throw @"perr" (ValueRedeclaration (declaration)) else pure (Map.insert name expression m)


checkNoUndefinedIdentifiers :: (HasThrow "perr" (ProductionError p) m) => (Map.Map Text (Expression p)) -> m (Map.Map Text (Expression p))
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


checkHasMain :: (HasThrow "perr" (ProductionError p) m) => Module p -> (Map.Map Text (Expression p)) -> m (Map.Map Text (Expression p))
checkHasMain moduleAST m = if isJust (Map.lookup "main" m) then pure m else throw @"perr" (NoMain moduleAST)

--convertToCPP :: Module p -> CPP.Expression P
--convertToCPP = 



example :: String
example = "module i `test module`\n\
          \test = 3\n\
          \rend = test\n\
          \"


produceProgram moduleAST = do
  identValMap <- createMapOfIdentifiersToValues moduleAST
  checkNoUndefinedIdentifiers identValMap
  checkHasMain moduleAST identValMap