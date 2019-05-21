module CopyPropagatedProgramConverter where
import ClassyPrelude
import Basic
import Data.Maybe
import Ast
import qualified CopyPropagatedProgram as CPP
import qualified Data.Map.Justified as Map
import qualified Data.Map as Unjustified.Map
import qualified Data.Set as Set
import Capability.Error

data Info = Info String
data Ty = TyVar Int
        | TyArr Ty Ty
        | TyAll String Ty
        | TySome String Ty
data Binding = NameBind
           | VarBind Ty
           | TyVarBind
data Term = TmVar Info Int Int
          | TmAbs Info String Ty Term
          | TmApp Info Term Term
          | TmTAbs Info String Term
          | TmTApp Info Term Ty
          | TmPack Info Ty Term Ty
          | TmUnpack Info String String Term Term

typeCheck = undefined

checkNoUnreferencedValues :: (HasThrow "perr" (ProductionError t p i) m, Ord i) => Module t p i -> m ()
checkNoUnreferencedValues (Module _ declarations _) = case allMissingReferences of
    [] -> pure ()
    _  -> throw @"perr" (UndeclaredValuesReferenced allMissingReferences)
  where
    allMissingReferences = allReferencesInModule >>= checkNoUnreferencedValuesInternal allTldsInModule

    allTldsInModule = Set.fromList $ fmap (\(Declaration (LowercaseIdentifier ident _) _ _) -> ident) declarations
    allReferencesInModule = (fmap (\(Declaration _ expression _) -> expression) declarations)

    checkNoUnreferencedValuesInternal vset (NumberLiteral i t p)
      = []
    checkNoUnreferencedValuesInternal vset (IdentifierExpression (LowercaseIdentifier ident p) _ _)
      = if ident `member` vset 
          then []
          else [LowercaseIdentifier ident p] 
    checkNoUnreferencedValuesInternal vset (FunctionLiteralExpression (FunctionLiteral identifiers (expression) _) _ _)
      = checkNoUnreferencedValuesInternal (vset <> (Set.fromList $ fmap (\(LowercaseIdentifier i _) -> i) identifiers)) expression
    checkNoUnreferencedValuesInternal vset (FunctionApplicationExpression fBeingApplied arguments _ _) 
      = (checkNoUnreferencedValuesInternal vset fBeingApplied) <> (arguments >>= checkNoUnreferencedValuesInternal vset)

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
    reduceExpression (FunctionLiteralExpression (FunctionLiteral boundIdentifiers subExpression _) _ _) = undefined
    reduceExpression e = e
    fullyReduce e = if reduceExpression e == e
                      then e
                      else fullyReduce (reduceExpression e)

example :: String
example = "module i `test module`\n\
          \test = -3.6\n\
          \myfunc = |a| -> (|b| -> a)\n\
          \main = test\n\
          \"

-- runM (parseModule example >>= produceProgram)
produceProgram moduleAST = do 
  checkNoUnreferencedValues moduleAST
  pure ()
  {-createMapOfIdentifiersToValues moduleAST $ \valueMap -> do 
    
    mainFunc <- getMainFunc moduleAST valueMap
    let (NumberLiteral n _ _) = evaluateMain valueMap mainFunc
    pure n-}
