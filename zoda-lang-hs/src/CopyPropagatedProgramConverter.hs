module CopyPropagatedProgramConverter where
import ClassyPrelude
import Basic
import Data.Maybe
import Ast
import qualified CopyPropagatedProgram as CPP
import qualified Data.Map.Justified as Map
import Capability.Error


data Metavariable = TypechecksOkay{-MetavariableApplication Int [Metavariable] 
                  | Metavariable Int 
                  | Constant Int-}

newtype Evaluatable t p i = Evaluatable ((Text, Expression t p i), [Evaluatable t p i])



checkValid :: forall t p i f. (Ord i, HasThrow "perr" (ProductionError t p i) f) => Module t p i -> f ()
checkValid modu =  case allUndefinedNames of
    x:_ -> throw @"perr" (UndeclaredValuesReferenced allUndefinedNames)
    _   -> pure ()
  where

    allUndefinedNames = getAllUndefinedNamesInModule modu
    
    getAllUndefinedNamesInModule m@(Module _ declarations _) = allUndefinedNames
      where 
        allTopLevelNames  = map (\case Declaration (LowercaseIdentifier i p) _ _ -> i) declarations
        allTopLevelValues = map (\case Declaration _ e _ -> e) declarations

        allUndefinedNames = allTopLevelValues >>= getUndefinedNamesUsedInExpression allTopLevelNames

    getUndefinedNamesUsedInExpression context (ParenthesizedExpression e _ _) = getUndefinedNamesUsedInExpression context e
    getUndefinedNamesUsedInExpression context (NumberLiteral _ _ _) = []
    getUndefinedNamesUsedInExpression context e@(IdentifierExpression identifier@(LowercaseIdentifier i _) _ _) = if i `elem` context then [] else [identifier]
    getUndefinedNamesUsedInExpression context (FunctionLiteralExpression (FunctionLiteral newIdentifiers expr _) _ _) = getUndefinedNamesUsedInExpression newContext expr
      where newContext = context <> (map (\case LowercaseIdentifier i _ -> i) newIdentifiers)


    {-


getMainFunc :: (Ord k, IsString k, HasThrow "perr" (ProductionError t p i) f) => Module t p i -> Map.Map ph k a -> f a
getMainFunc moduleAST m = case "main" `Map.member` m of
  Nothing -> throw @"perr" (NoMain moduleAST)
  Just i -> pure $ Map.lookup i m


--typeCheck :: (Ord k, IsString k, HasThrow "perr" (ProductionError t p i) f) => Map.Map ph k (Module u p i) -> m (Map.Map ph k (Module Metavariable p i))
{-typeCheck m = (flip mapM) m $ \case 
  FunctionLiteralExpression (FunctionLiteral t p i) t p -> pure TypechecksOkay
_ -> pure TypechecksOkay-}

evaluateMain :: (Ord k, Eq t, Eq p) => Map.Map ph k (Expression t p (Map.Key ph k)) -> Ast.Expression t p (Map.Key ph k) -> Ast.Expression t p (Map.Key ph k)
evaluateMain identValMap mainFunc =  fullyReduce mainFunc
  where
    reduceExpression e@(NumberLiteral _ _ _) = e
    reduceExpression (IdentifierExpression (LowercaseIdentifier ident _) _ _) = Map.lookup ident identValMap
    reduceExpression (FunctionLiteralExpression (FunctionLiteral idents expr p1) t p2) = FunctionLiteralExpression (FunctionLiteral idents expr p1) t p2
    reduceExpression e = e
    fullyReduce e = if reduceExpression e == e
                      then e
                      else fullyReduce (reduceExpression e)


-- runM (parseModule example >>= produceProgram)
-}
produceProgram moduleAST = do 
  checkValid moduleAST
  pure undefined



example :: String
example = "module i `test module`\n\
          \test = -3.6\n\
          \main = |a| a\n\
          \"
