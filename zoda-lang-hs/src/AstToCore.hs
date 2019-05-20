module AstToCore where
import ClassyPrelude
import Capability.Error
import Data.Functor.Foldable (cata, ListF(..))
import qualified Data.Map.Justified as Map
import qualified Data.Map as Unjustified.Map

import Basic
import Ast 
import qualified Core


createMapOfIdentifiersToValues :: (HasThrow "perr" (ProductionError t p i) m, Ord i) => Module t p i -> (forall ph. Map.Map ph i (Expression t p (Map.Key ph i)) -> m out) -> m out
createMapOfIdentifiersToValues (Module _ declarations _) continuation = handleMapOutput mapOutput 
  where
    mapOutput = Map.withRecMap (Unjustified.Map.fromList declarationList) continuation
    handleMapOutput (Left missingReferences) = throw @"perr" . UndeclaredValuesReferenced $ fmap convertError missingReferences
      where convertError (k, f) = (k, fmap fst f)
    handleMapOutput (Right output) = output
    extractDec (Declaration (LowercaseIdentifier identifierName _) expression _) = (identifierName, expression)
    declarationList = fmap extractDec declarations 

withAst :: (HasThrow "perr" (ProductionError t p i) m, Ord i) => Module t p i 
        -> (forall ph. Core.Program ph t p i -> m out)
        -> m out
withAst ast f = createMapOfIdentifiersToValues ast (f . mapToCore) 

mapToCore :: Map.Map ph i (Expression t p (Map.Key ph i)) -> Core.Program ph t p i 
mapToCore exprMap = Core.Program (fmap (cata mapToCoreF) exprMap)
  where 
    mapToCoreF :: ExpressionF t p (Map.Key ph i) (Core.Expression ph t p i) 
               -> (Core.Expression ph t p i)
    mapToCoreF (NumberLiteralF i t p)                   = Core.NumberLiteral i t p
    mapToCoreF (IdentifierExpressionF ident t p)        = Core.IdentifierExpression ident t p
    mapToCoreF (FunctionLiteralExpressionF flit t p)    = Core.LambdaExpression (error "not implemented yet") t p
    mapToCoreF (FunctionApplicationExpressionF _ _ _ _) = error "not implemented yet"