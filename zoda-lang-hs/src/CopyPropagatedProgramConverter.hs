module CopyPropagatedProgramConverter where
import Errors
import Control.Monad
import Data.Maybe
import Ast
import qualified CopyPropagatedProgram as CPP
import qualified Data.Map as Map
import Capability.Error

createMapOfIdentifiersToValues :: Module p -> Either (ProductionError p) (Map.Map String (Expression p))
createMapOfIdentifiersToValues (Module header declarations _) = valueMap
 where
  valueMap = foldM addValueToValuemap Map.empty declarations
  addValueToValuemap m declaration@(Declaration (LowercaseIdentifier name _) expression _) =
    if isJust (Map.lookup name m) then Left (ValueRedeclaration (declaration)) else pure (Map.insert name expression m)


checkNoUndefinedIdentifiers :: (Map.Map String (Expression p)) -> Either (ProductionError p) (Map.Map String (Expression p))
checkNoUndefinedIdentifiers m = mapM (checkIdentifiersInExpressionDefined m) (fmap snd . Map.toList $ m) *> pure m
 where
  getIdentifiersInExpression (NumberLiteralExpression _          _) = []
  getIdentifiersInExpression (IdentifierExpression    identifier _) = [identifier]
  checkIdentifierDefined m identifier@(LowercaseIdentifier s _) = isJust lookedUp where lookedUp = Map.lookup s m
  checkIndentifiersDefined :: (Map.Map String (Expression p)) -> [LowercaseIdentifier p] -> Either (ProductionError p) [LowercaseIdentifier p]
  checkIndentifiersDefined m identifiers = mapM getIdentifierDefinedOrError identifiers
    where getIdentifierDefinedOrError identifier = if checkIdentifierDefined m identifier then pure identifier else Left (UndeclaredValueReferenced identifier)
  checkIdentifiersInExpressionDefined :: (Map.Map String (Expression p)) -> Expression p -> Either (ProductionError p) Bool
  checkIdentifiersInExpressionDefined m e = checkIndentifiersDefined m (getIdentifiersInExpression e) *> pure True


checkHasMain :: Module p -> (Map.Map String (Expression p)) -> Either (ProductionError p) (Map.Map String (Expression p))
checkHasMain modu m = if isJust (Map.lookup "main" m) then pure m else Left (NoMain modu)

--convertToCPP :: Module p -> CPP.Expression P
--convertToCPP = 



example :: String
example = "module i `test module`\n\
          \test = 3\n\
          \rend = test\n\
          \"