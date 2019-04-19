module CopyPropagatedProgramConverter where
import ClassyPrelude
import Basic
import Data.Maybe
import Ast
import qualified CopyPropagatedProgram as CPP
import qualified Data.Map.Justified as Map
import qualified Data.Map as Unjustified.Map
import Capability.Error

createMapOfIdentifiersToValues :: (HasThrow "perr" (ProductionError p i) m, Ord i) => Module p i -> m (Unjustified.Map.Map i (Expression p i))
createMapOfIdentifiersToValues (Module header declarations _) = undefined {-valueMap
 where
  valueMap = (foldM addValueToValuemap Unjustified.Map.empty declarations)
addValueToValuemap m declaration@(Declaration (LowercaseIdentifier name _) expression _) =
  if isJust (Unjustified.Map.lookup name m) then throw @"perr" (ValueRedeclaration (declaration)) else pure (Unjustified.Map.insert name expression m)-}


checkNoUndefinedIdentifiers :: (HasThrow "perr" (ProductionError p i) m, Ord i) => (Unjustified.Map.Map i (Expression p i)) -> m ()
checkNoUndefinedIdentifiers m = undefined{-mapM (checkIdentifiersInExpressionDefined m) (fmap snd . Unjustified.Map.toList $ m) *> pure ()
 where
  getIdentifiersInExpression (NumberLiteralExpression _          _) = []
  getIdentifiersInExpression (IdentifierExpression    identifier _) = [identifier]
  checkIndentifiersDefined ::  (HasThrow "perr" (ProductionError p i) m, Ord i) => (Unjustified.Map.Map i (Expression p i)) -> [LowercaseIdentifier p i] -> m [LowercaseIdentifier p i]
  checkIndentifiersDefined m identifiers = mapM getIdentifierDefinedOrError identifiers
    where 
      getIdentifierDefinedOrError :: (HasThrow "perr" (ProductionError p i) m, Ord i) => LowercaseIdentifier p i -> m ()
      getIdentifierDefinedOrError identifier = if identifier `Unjustified.Map.member` m then pure () else throw @"perr" (UndeclaredValueReferenced identifier)
  checkIdentifiersInExpressionDefined :: (HasThrow "perr" (ProductionError p i) m, Ord i) => (Unjustified.Map.Map i (Expression p i)) -> Expression p i -> m ()
  checkIdentifiersInExpressionDefined m e = checkIndentifiersDefined m (getIdentifiersInExpression e) *> pure ()-}

createMapOfIdentifiersToValues' [declarations] = Map.withRecMap (Unjustified.Map.fromList declarationList) (\i -> traceShow i ())
  where
    extractDec (Declaration (LowercaseIdentifier identifierName _) expression _) = (identifierName, expression)
    declarationList = fmap extractDec declarations 


getMainFunc :: (HasThrow "perr" (ProductionError p Text) m) => Module p Text -> (Unjustified.Map.Map Text (Expression p Text)) -> m (Expression p Text)
getMainFunc moduleAST m = handleMainFunc mainFunc
  where
    mainFunc = Unjustified.Map.lookup "main" m
    handleMainFunc (Just f) = pure f
    handleMainFunc (Nothing) = throw @"perr" (NoMain moduleAST)



evaluateMain :: (Eq p, Ord i) => (Unjustified.Map.Map i (Expression p i)) -> Expression p i -> Expression p i
evaluateMain identValMap mainFunc = fullyReduce mainFunc
  where
    --reduceExpression :: Expression p -> Expression p
    reduceExpression e@(NumberLiteralExpression _ _) = e 
    reduceExpression (IdentifierExpression (LowercaseIdentifier ident _) _) = e
      where
        (Just e) = (Unjustified.Map.lookup ident identValMap)
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