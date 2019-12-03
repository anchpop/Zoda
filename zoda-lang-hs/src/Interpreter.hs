module Interpreter (produceProgram) where
import ClassyPrelude hiding (lookup)
import Data.List (lookup)
import Basic 
import Data.Maybe
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

produceProgram :: Text -> Either (ProductionError Untyped SourcePosition (Text, SourcePosition) Text) Rational
produceProgram input = join (parsedModule >>= (\x -> copyPropagated primatives x (applicant x)))
  where
    parsedModule = parseModule input
    applicant moduOriginal modu = do
      typecheck modu
      case "main" `Map.member` modu of
        Just mainValueKey -> fmap (\(NumberLiteral r _ _) -> r) (pure $ normalize modu' [] (normalizeExprMetadata $ mainValueKey `Map.lookup` modu) (NatTypeExpression () ())) 
        _                 -> Left $ NoMain moduOriginal
      where modu' = fmap normalizeExprMetadata modu

np = NoBind ()


primatives = [
    ("Type", UniverseExpression 1 Untyped Base),
    ("Nat", NatTypeExpression Untyped Base)
  ]