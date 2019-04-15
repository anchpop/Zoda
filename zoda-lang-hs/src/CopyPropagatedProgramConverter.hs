module CopyPropagatedProgramConverter where
import Errors
import Control.Monad
import Data.Maybe
import Ast
import qualified CopyPropagatedProgram as CPP
import qualified Data.Map as Map

createMapOfIdentifiersToValues :: Module p -> Either (ProductionError p) (Map.Map String (Expression p))
createMapOfIdentifiersToValues (Module header declarations _) = valueMap
 where
  --valueMap :: Either (ProductionError p) (Map.Map String (Expression a))
  valueMap = foldM addValueToValuemap Map.empty declarations
  --addValueToValuemap
    -- :: (Map.Map String (Expression a)) -> Declaration a -> Either (ProductionError a) (Map.Map String (Expression a))
  addValueToValuemap m declaration@(Declaration (LowercaseIdentifier name _) expression _) =
    if isJust (Map.lookup name m) then Left (ValueRedeclaration (declaration)) else pure (Map.insert name expression m)


checkNoUndefinedIdentifiers
  :: (Map.Map String (Expression p)) -> Either (ProductionError p) (Map.Map String (Expression p))
checkNoUndefinedIdentifiers m = mapM (checkIdentifiersInExpressionDefined m) (fmap snd . Map.toList $ m) *> pure m
 where
  getIdentifiersInExpression (NumberLiteralExpression _          _) = []
  getIdentifiersInExpression (IdentifierExpression    identifier _) = [identifier]
  checkIdentifierDefined m identifier@(LowercaseIdentifier s _) = isJust lookedUp where lookedUp = Map.lookup s m
  checkIndentifiersDefined
    :: (Map.Map String (Expression p)) -> [LowercaseIdentifier p] -> Either (ProductionError p) [LowercaseIdentifier p]
  checkIndentifiersDefined m identifiers = filterM getIdentifierDefinedOrError identifiers
   where
    getIdentifierDefinedOrError identifier =
      if checkIdentifierDefined m identifier then pure True else Left (UndeclaredValueReferenced identifier)
  checkIdentifiersInExpressionDefined
    :: (Map.Map String (Expression p)) -> Expression p -> Either (ProductionError p) Bool
  checkIdentifiersInExpressionDefined m e = checkIndentifiersDefined m (getIdentifiersInExpression e) *> pure True


example :: String
example = "module i `test module`\n\
          \test = 3\n\
          \rend = test\n\
          \"