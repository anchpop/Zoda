module CopyPropagatedProgramConverter where
import ClassyPrelude
import Basic
import Data.Maybe
import Ast
import qualified CopyPropagatedProgram as CPP
import Capability.Error


data Metavariable = TypechecksOkay{-MetavariableApplication Int [Metavariable] 
                  | Metavariable Int 
                  | Constant Int-}

newtype Evaluatable t p i = Evaluatable ((Text, Expression t p i), [Evaluatable t p i])

data Types = Number | Bool | Arr Types Types deriving (Eq, Show, Read)


synth context (NumberLiteral e t _) = pure Number
synth context (Annotation e t _) = check context e t *> pure t
synth context (IdentifierExpression (LowercaseIdentifier x _) _ _) = case lookup x context of 
  Just t  -> pure t 
  Nothing -> Left "Type synthesis error bruh"
synth context (FunctionApplicationExpression fun [arg] _ _) = do
  functionType <- synth context fun
  case functionType of 
    Arr t1 t2 -> check context arg t1 *> pure t2
    _         -> Left "Type synthesis error bruh"
check context (FunctionLiteralExpression [LowercaseIdentifier x _] (body) _ _) (Arr t1 t2) = check ((x, t1):context) body t2
check context (FunctionLiteralExpression [LowercaseIdentifier x _] (body) _ _) _ = Left "Type checking error bruh"
check context expression against = do
  t <- synth context expression
  pure $ t == against
   


data IdentifierMeaning t p i = Ref (Expression t p i) | Free | NotAReference deriving (Eq, Ord, Read, Show)

listLookup toLookup [] = Nothing
listLookup toLookup ((k, v):xs)
    | k == toLookup = Just v
    | otherwise = listLookup toLookup xs


checkNamesDefined :: forall t p i. Ord i => Module t p i -> Module t p (i, Maybe (IdentifierMeaning t p i))
checkNamesDefined = copyPropagate
  where

    copyPropagate ::  Module t p i -> (Module t p (i, Maybe (IdentifierMeaning t p i)))
    copyPropagate (Module (ModuleHeader (LowercaseIdentifier i1 p1) (Tinydoc text p2) p3) declarations p4) = Module (ModuleHeader (LowercaseIdentifier (i1, Just NotAReference) p1) (Tinydoc text p2) p3) propagatedDeclarations p4
      where 
        allTopLevelValues = map (\case Declaration (LowercaseIdentifier i _) e _ -> (i, Ref e)) declarations

        propagatedDeclarations ::  [Declaration t p (i, Maybe (IdentifierMeaning t p i))]
        propagatedDeclarations = map (propagateDeclaration allTopLevelValues) declarations
        
    propagateDeclaration :: [(i, IdentifierMeaning t p i)] -> Declaration t p i -> Declaration t p (i, Maybe (IdentifierMeaning t p i))
    propagateDeclaration context (Declaration (LowercaseIdentifier i p1) expression p2) = 
      Declaration (LowercaseIdentifier (i, Just NotAReference) p1) propagatedExpression p2
        where 
          propagatedExpression :: Expression t p (i, Maybe (IdentifierMeaning t p i))
          propagatedExpression = propagateExpression context expression

    propagateExpression :: [(i, IdentifierMeaning t p i)] -> Expression t p i -> Expression t p (i, Maybe (IdentifierMeaning t p i))
    propagateExpression context (ParenthesizedExpression e p t) = ParenthesizedExpression (propagateExpression context e) p t
    propagateExpression context (NumberLiteral rational t p) = NumberLiteral rational t p
    propagateExpression context e@(IdentifierExpression identifier@(LowercaseIdentifier i p) t p2) = 
      IdentifierExpression (LowercaseIdentifier (i, i `listLookup` context) p) t p2
    propagateExpression context (FunctionLiteralExpression parameters expr t p) = FunctionLiteralExpression newParameters (propagateExpression newContext expr) t p
      where newParameters = (map (\case LowercaseIdentifier i p -> LowercaseIdentifier (i, Just NotAReference) p) parameters)
            newContext    = (map (\case LowercaseIdentifier i _ -> (i, Free)) parameters) <> context 
    propagateExpression context (FunctionApplicationExpression e v t p) = FunctionApplicationExpression (propagateExpression context e) (map (propagateExpression context) v) t p
    propagateExpression context (Annotation e t p) = Annotation (propagateExpression context e) t p



getMainFunc :: forall t p i. (IsString i, Ord i, Ord t, Eq p) => Module t p (i, Maybe (IdentifierMeaning t p i)) -> Maybe (Expression t p (i, Maybe (IdentifierMeaning t p i)))
getMainFunc m@(Module _ declarations _) = map (\(Ref (e)) -> e) (lookup ("main", Just NotAReference) allTopLevelValues)
  where
    allTopLevelValues = map (\case Declaration (LowercaseIdentifier i p) e _ -> (i, Ref e)) declarations

{-

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
  let propagated = checkNamesDefined moduleAST
      mainfunc = getMainFunc propagated
  case mainfunc of 
    Nothing -> throw @"perr" (NoMain moduleAST)
    Just e -> evaluate e
  where 
    evaluate e = pure e



example :: String
example = "module i `test module`\n\
          \test = (-3.6)\n\
          \main = test.(|a| a)\n\
          \"
