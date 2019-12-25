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


copyPropagated :: forall i p t o. (Show i, Show t, Show p, NominalShow i, NominalShow t, NominalShow p, Ord i, Bindable i, Bindable p, Bindable t) => [(i, DeclarationInfo t p (i, p) i i)] -> Module t p (i, p) i -> (forall ph. JustifiedModule t p ph i i i -> o) -> Either (ProductionError t p (i, p) i) o
copyPropagated prims (Module _ declarations _) f = Map.withMap dUMap (\m -> f <$> dJmapToJustifiedModule m)
  where
    dUMap = UMap.fromList (prims <> (declarations >>= (\case 
        ValueDefinition          identifier expression _                -> [(identifier, Value expression)]
        ValueDefinitionAnnotated identifier expression _ annotation _ _ -> [(identifier, ValueAndAnnotation expression annotation)]
        TypeDefinition typName typType _ constructors _                 -> (typName, TypeConstructor typName typType):(fmap (\(index, (constName, constType, _)) -> (constName, DataConstructor index constType)) (zip [0..] constructors))
      )))
    dJmapToJustifiedModule :: (Map.Map ph i (DeclarationInfo t p (i, p) i i')) -> Either (ProductionError t p (i, p) i) (JustifiedModule t p ph i i i')
    dJmapToJustifiedModule m = 
      for m justifyDeclarationInfo
      where justifyExpression e = forExpr2 e (justifyReferences m)
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
    justifyReferences :: Map.Map ph i c -> (i, p) ->  Either (ProductionError t p m i) (Map.Key ph i)
    justifyReferences referenceMap (iJustified, pJustified) = case iJustified `Map.member` referenceMap of 
      Nothing -> Left $ UndeclaredValuesReferenced [(iJustified, pJustified)] 
      Just k  -> pure k 



produceProgram :: Text -> Either (ProductionError Untyped SourcePosition (Text, SourcePosition) Text) Rational
produceProgram input = join (parsedModule >>= (\x -> copyPropagated primatives x (applicant x)))
  where
    parsedModule = parseModule input
    applicant moduOriginal = v 
      where v :: forall ph. JustifiedModule Untyped SourcePosition ph Text Text Text -> Either (ProductionError Untyped SourcePosition (Text, SourcePosition) Text) Rational
            v modu = do
              typecheck modu
              case "main" `Map.member` modu of
                Just mainValueKey -> fmap (\(NumberLiteral r _ _) -> r) (pure $ normalize modu' [] (parsedToNormalizedPlain . getValueFromDeclarationInfo $ mainValueKey `Map.lookup` modu) (NatTypeExpression () ())) 
                _                 -> Left $ NoMain moduOriginal
              where modu' :: JustifiedModule () () ph Text () Text
                    modu' = fmap normalizeDeclarationInfoMetadata modu

np :: NoBind ()
np = NoBind ()

primatives :: [(Text, DeclarationInfo Untyped SourcePosition m i Text)]
primatives = [
    ("Type", Value (UniverseExpression 1 Untyped Base)),
    ("Nat", Value (NatTypeExpression Untyped Base))
  ]