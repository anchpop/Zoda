module CopyPropagatedProgramConverter where
import ClassyPrelude
import Basic
import Data.Maybe
import Ast
import qualified CopyPropagatedProgram as CPP
import qualified Data.Map.Justified as Map
import qualified Data.Map as Unjustified.Map
import Capability.Error

createMapOfIdentifiersToValues :: (HasThrow "perr" (ProductionError p i) m, Ord i)	=> Module p i -> (forall ph. Map.Map ph i (Expression p (Map.Key ph i)) -> m t) -> m t
createMapOfIdentifiersToValues (Module _ declarations _) continuation = handleMapOutput mapOutput 
  where
    mapOutput = Map.withRecMap (Unjustified.Map.fromList declarationList) continuation
    handleMapOutput (Left missingReferences) = throw @"perr" . UndeclaredValuesReferenced $ fmap convertError missingReferences
      where convertError (k, f) = (k, fmap fst f)
    handleMapOutput (Right output) = output
    extractDec (Declaration (LowercaseIdentifier identifierName _) expression _) = (identifierName, expression)
    declarationList = fmap extractDec declarations 


getMainFunc moduleAST m = case "main" `Map.member` m of
  Nothing -> throw @"perr" . NoMain $ moduleAST
  Just i -> pure $ Map.lookup i m


evaluateMain identValMap mainFunc =  fullyReduce mainFunc
  where
    reduceExpression e@(NumberLiteralExpression _ _) = e 
    reduceExpression (IdentifierExpression (LowercaseIdentifier ident _) _) = Map.lookup ident identValMap
    reduceExpression e = e
    fullyReduce e = if reduceExpression e == e 
                      then e
                      else fullyReduce (reduceExpression e)



example :: String
example = "module i `test module`\n\
          \test = -3.6\n\
          \main = test\n\
          \"


produceProgram moduleAST = do
  createMapOfIdentifiersToValues moduleAST $ \valueMap -> do 
    mainFunc <- getMainFunc moduleAST valueMap
    let (NumberLiteralExpression (NumberLiteral n _) _) = evaluateMain valueMap mainFunc
    pure n
    --traceShow valueMap (pure ())
  {-identValMap <- createMapOfIdentifiersToValues moduleAST
  checkNoUndefinedIdentifiers identValMap
  mainFunc <- getMainFunc moduleAST identValMap
  pure $ evaluateMain identValMap mainFunc-}
-- use with `runM (parseModule example >>= produceProgram)`