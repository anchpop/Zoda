module Interpreter (produceProgram, primatives, copyPropagated) where
import ClassyPrelude hiding (lookup)
import Data.List (lookup)
import Basic 
import Ast
import CopyPropagatedProgram
import qualified Data.Set as Set
import qualified Data.Map.Justified as Map
import qualified Data.Map.Lazy as UMap
import Nominal hiding ((.))
import Data.Void
import Data.Ratio
import Typechecker
import Normalizer
import Parser
import qualified Data.List.NonEmpty as NonEmpty


copyPropagated :: forall i p t o. (Ord i, Bindable i, Bindable p, Bindable t) => [(i, (Expression t p (i, p) i, Maybe (Expression t p (i, p) i)))] -> Module t p (i, p) i -> (forall ph. JustifiedModule t p ph i i -> o) -> Either (ProductionError t p (i, p) i) o
copyPropagated prims (Module _ declarations _) f = Map.withMap dUMap (\m -> f <$> dJmapToJustifiedModule m)
  where
    dUMap = UMap.fromList (prims <> catMaybes (fmap (\case 
        ValueDefinition          identifier expression _              -> Just (identifier, (expression, Nothing))
        ValueDefinitionAnnotated identifier expression _ annotation _ -> Just (identifier, (expression, Just annotation))
        _                                       -> Nothing
      ) declarations))
    dJmapToJustifiedModule :: (Map.Map ph i (Expression t p (i, p) i, Maybe (Expression t p (i, p) i))) -> Either (ProductionError t p (i, p) i) (JustifiedModule t p ph i i)
    dJmapToJustifiedModule m = 
      for m (\(v, annotation) -> do 
        v' <- justifyExpression v
        annotation' <- traverse justifyExpression annotation
        pure (v', annotation'))
      where justifyExpression e = forExpr2 e (justifyReferences m)
    justifyReferences :: Map.Map ph i c -> (i, p) ->  Either (ProductionError t p m i) (Map.Key ph i)
    justifyReferences referenceMap (iJustified, pJustified) = case iJustified `Map.member` referenceMap of 
      Nothing -> Left $ UndeclaredValuesReferenced [(iJustified, pJustified)] 
      Just k  -> pure k 



produceProgram :: Text -> Either (ProductionError Untyped SourcePosition (Text, SourcePosition) Text) Rational
produceProgram input = join (parsedModule >>= (\x -> copyPropagated primatives x (applicant x)))
  where
    parsedModule = parseModule input
    applicant moduOriginal modu = do
      typecheck modu
      case "main" `Map.member` modu of
        Just mainValueKey -> fmap (\(NumberLiteral r _ _) -> r) (pure $ normalize modu' [] (normalizeExprMetadata . fst $ mainValueKey `Map.lookup` modu) (NatTypeExpression () ())) 
        _                 -> Left $ NoMain moduOriginal
      where modu' = fmap normalizeExprAnnotationMetadata modu

np :: NoBind ()
np = NoBind ()

primatives :: [(Text, (Expression Untyped SourcePosition m i, Maybe (Expression Untyped SourcePosition m i)))]
primatives = [
    ("Type", (UniverseExpression 1 Untyped Base, Nothing)),
    ("Nat", (NatTypeExpression Untyped Base, Nothing))
  ]