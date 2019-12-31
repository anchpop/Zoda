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


copyPropagated :: forall i o. (Show i, NominalShow i, Ord i, Bindable i) => [(i, DeclarationInfo Parsed i (i, SourcePosition) i)] -> Module Parsed i (i, SourcePosition) -> (forall ph. JustifiedModule Parsed ph i i i -> o) -> Either (ProductionError i (i, SourcePosition)) o
copyPropagated prims (Module _ declarations) f = dUMap >>= (\dUMap' -> Map.withMap dUMap' (\m -> f <$> dJmapToJustifiedModule m))
  where
    dUMap :: Either (ProductionError i (i, SourcePosition)) (Map i (DeclarationInfo Parsed i (i, SourcePosition) i))
    dUMap = case duplicateDeclarations of 
        [] -> pure $ (UMap.fromList) allDelcarations
        x  -> Left $ ValueRedeclaration x
      where allDelcarations = (prims <>) $ declarations >>= \case 
                                ValueDefinitionX          _ identifier expression             -> [(identifier, Value expression)] :: [(i, DeclarationInfo Parsed i (i, SourcePosition) i)]
                                ValueDefinitionAnnotatedX _ identifier expression annotation  -> [(identifier, ValueAndAnnotation expression annotation)]
                                TypeDefinitionX           _ typName typType constructors      -> 
                                  (typName, TypeConstructor typName typType):(fmap (\(index, (_, constName, constType)) -> (constName, DataConstructor index constType)) (zip [0..] constructors))
            duplicateDeclarations = filter (\x -> length x > 1) ((groupBy (\x y -> fst x == fst y)) (sortBy (\x y -> compare (fst x) (fst y)) allDelcarations))
    
    dJmapToJustifiedModule :: forall ph. (Map.Map ph i (DeclarationInfo Parsed i (i, SourcePosition) i)) -> Either (ProductionError i (i, SourcePosition)) (JustifiedModule Parsed ph i i i)
    dJmapToJustifiedModule m = 
      for m justifyDeclarationInfo
      where justifyExpression :: ExpressionX Parsed i (i, SourcePosition) -> Either (ProductionError i (i, SourcePosition)) (ExpressionX Parsed i (Map.Key ph i))
            justifyExpression e = forExpr1 e (justifyReferences m)
            justifyDeclarationInfo :: DeclarationInfo Parsed i (i, SourcePosition) i -> Either (ProductionError i (i, SourcePosition)) (JustifiedDeclarationInfo Parsed ph i i i)
            justifyDeclarationInfo (Value v) = do 
              v' <- justifyExpression v
              pure $ Value v'
            justifyDeclarationInfo (ValueAndAnnotation v t) = do 
              v' <- justifyExpression v
              t' <- justifyExpression t
              pure $ ValueAndAnnotation v' t'
            justifyDeclarationInfo (TypeConstructor n t) = do 
              t' <- justifyExpression t
              pure $ TypeConstructor n t'
            justifyDeclarationInfo (DataConstructor i t) = do 
              t' <- justifyExpression t
              pure $ DataConstructor i t'
    justifyReferences :: forall ph. (Map.Map ph i (DeclarationInfo Parsed i (i, SourcePosition) i)) -> (i, SourcePosition) -> Either (ProductionError i (i, SourcePosition)) (Map.Key ph i)
    justifyReferences referenceMap (iJustified, pJustified) = case iJustified `Map.member` referenceMap of 
      Nothing -> Left $ (UndeclaredValuesReferenced [(iJustified, pJustified)] :: ProductionError i (i, SourcePosition))
      Just k  -> pure k 



produceProgram :: Text -> Either (ProductionError Text (Text, SourcePosition)) Rational
produceProgram input = join (parsedModule >>= (\x -> copyPropagated primatives x (applicant x)))
  where
    parsedModule = parseModule input
    applicant moduOriginal modu = do
      typecheck modu
      case "main" `Map.member` modu of
        Just mainValueKey -> fmap (\(NumberLiteralX _ r) -> r) (pure $ normalize modu' [] (parsedToNormalizedPlain . getValueFromDeclarationInfo $ mainValueKey `Map.lookup` modu) (NatTypeExpression)) 
        _                 -> Left $ NoMain moduOriginal
      where modu' = fmap normalizeDeclarationInfoMetadata modu

np :: NoBind ()
np = NoBind ()

primatives :: [(Text, DeclarationInfo Parsed Text m n)]
primatives = [
    ("Type", Value (UniverseExpressionX (Sp Base) 1)),
    ("Nat", Value (NatTypeExpressionX (Sp Base)))
  ]