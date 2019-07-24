module CopyPropagatedProgramConverter where
import ClassyPrelude
import Basic
import Data.Maybe
import Ast
import qualified CopyPropagatedProgram as CPP
import Capability.Error
import qualified Data.Set as Set

data Metavariable = TypechecksOkay{-MetavariableApplication Int [Metavariable] 
                  | Metavariable Int 
                  | Constant Int-}

newtype Evaluatable t p i = Evaluatable ((Text, Expression t p i), [Evaluatable t p i])

data Types = Number | Bool | Arr Types Types deriving (Eq, Show, Read)


data IdentifierMeaning t p i = Ref (Expression t p (i, Maybe (IdentifierMeaning t p i))) | LambdaVar | NotAReference deriving (Eq, Ord, Read, Show)

--synth :: (Eq a1, IsString a2) => [(a1, Types)] -> Expression Types p (a1, Maybe (IdentifierMeaning Types p a1)) -> Either a2 Types
synth context (NumberLiteral _ _ _) = pure Number
synth context (Annotation e (IdentifierExpression (LowercaseIdentifier ("Number", _) _) _ _) _ _) = check context e Number *> pure Number -- check context e t *> pure t
synth context (Annotation e (IdentifierExpression (LowercaseIdentifier ("Bool",   _) _) _ _) _ _) = check context e Bool *> pure Bool -- check context e t *> pure t
synth context (Annotation e (TArrow e1 e2 _ _) _ _) = do
  t1 <- synth context e1
  t2 <- synth context e2
  let t = t1 `Arr` t2
  check context e t 
  pure t
synth context (IdentifierExpression (LowercaseIdentifier (x, Just LambdaVar) _) _ _) = case lookup x context of 
  Just t  -> pure t 
  Nothing -> Left "Type synthesis error bruh"
synth context (IdentifierExpression (LowercaseIdentifier (_, Just (Ref x)) _) _ _) = synth [] x
synth context (FunctionApplicationExpression fun [arg] _ _) = do
  functionType <- synth context fun
  case functionType of 
    Arr t1 t2 -> check context arg t1 *> pure t2
    _         -> Left "Type synthesis error bruh"
synth context (ParenthesizedExpression e t p) = synth context e
synth _ x = error (show x)
check context (FunctionLiteralExpression [LowercaseIdentifier (x, _) _] (body) _ _) (Arr t1 t2) = check ((x, t1):context) body t2
check context (FunctionLiteralExpression [LowercaseIdentifier (x, _) _] (body) _ _) _ = Left "Type checking error bruh"
check context expression against = do
  t <- synth context expression
  pure $ t == against

   


listLookup toLookup [] = Nothing
listLookup toLookup ((k, v):xs)
    | k == toLookup = Just v
    | otherwise = listLookup toLookup xs


checkNamesDefined :: forall t p i. (Ord i, IsString i, Show i, Show t, Show p) => Module t p i -> Module t p (i, Maybe (IdentifierMeaning t p i))
checkNamesDefined = copyPropagate
  where

    copyPropagate ::  Module t p i -> (Module t p (i, Maybe (IdentifierMeaning t p i)))
    copyPropagate (Module (ModuleHeader (LowercaseIdentifier i1 p1) (Tinydoc text p2) p3) declarations p4) = traceShow allTopLevelTypechecks $ Module (ModuleHeader (LowercaseIdentifier (i1, Just NotAReference) p1) (Tinydoc text p2) p3) propagatedDeclarations p4
      where 
        allTopLevelValues = map (\case d@(Declaration (LowercaseIdentifier i _) e _) -> (i, Ref (propagateExpression allTopLevelValues e))) declarations
        allTopLevelTypechecks = map ((synth []) . \case (_, Ref x) -> x) allTopLevelValues

        propagatedDeclarations ::  [Declaration t p (i, Maybe (IdentifierMeaning t p i))]
        propagatedDeclarations = map (propagateDeclaration allTopLevelValues) declarations
        
        propagateDeclaration :: [(i, IdentifierMeaning t p i)] -> Declaration t p i -> Declaration t p (i, Maybe (IdentifierMeaning t p i))
        propagateDeclaration context (Declaration (LowercaseIdentifier i p1) expression p2) = 
          Declaration (LowercaseIdentifier (i, Just NotAReference) p1) propagatedExpression p2
            where 
              propagatedExpression :: Expression t p (i, Maybe (IdentifierMeaning t p i))
              propagatedExpression = propagateExpression context expression

propagateExpression :: forall t i p. (Eq i) => [(i, IdentifierMeaning t p i)] -> Expression t p i -> Expression t p (i, Maybe (IdentifierMeaning t p i))
propagateExpression context (ParenthesizedExpression e p t) = ParenthesizedExpression (propagateExpression context e) p t
propagateExpression context (NumberLiteral rational t p) = NumberLiteral rational t p
propagateExpression context e@(IdentifierExpression identifier@(LowercaseIdentifier i p) t p2) = 
  IdentifierExpression (LowercaseIdentifier (i, i `listLookup` context) p) t p2
propagateExpression context (FunctionLiteralExpression parameters expr t p) = FunctionLiteralExpression newParameters (propagateExpression newContext expr) t p
  where newParameters = (map (\case LowercaseIdentifier i p -> LowercaseIdentifier (i, Just NotAReference) p) parameters)
        newContext    = (map (\case LowercaseIdentifier i _ -> (i, LambdaVar)) parameters) <> context 
propagateExpression context (FunctionApplicationExpression e v t p) = FunctionApplicationExpression (propagateExpression context e) (map (propagateExpression context) v) t p
propagateExpression context (Annotation e1 e2 t p) = Annotation (propagateExpression context e1) (propagateExpression context e2) t p

depropagateExpression :: Expression t p (i, Maybe (IdentifierMeaning t p i)) -> Expression t p i
depropagateExpression (ParenthesizedExpression e p t) = ParenthesizedExpression (depropagateExpression e) p t
depropagateExpression (NumberLiteral rational t p) = NumberLiteral rational t p
depropagateExpression e@(IdentifierExpression (LowercaseIdentifier (i, _) p) t p2) = 
  IdentifierExpression (LowercaseIdentifier i p) t p2
depropagateExpression (FunctionLiteralExpression parameters expr t p) = FunctionLiteralExpression newParameters (depropagateExpression expr) t p
  where newParameters = (map (\case LowercaseIdentifier (i, _) p -> LowercaseIdentifier i p) parameters)
depropagateExpression (FunctionApplicationExpression e v t p) = FunctionApplicationExpression (depropagateExpression e) (map depropagateExpression v) t p
depropagateExpression (Annotation e1 e2 t p) = Annotation (depropagateExpression e1) (depropagateExpression e2) t p


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
produceProgram :: forall t p i m. (IsString i, Ord i, Ord t, Eq p, Show i, Show t, Show p, HasThrow "perr" (ProductionError t p i) m) => Module t p i -> m (Expression t p (i, Maybe (IdentifierMeaning t p i)))
produceProgram moduleAST = 
  let propagated = checkNamesDefined moduleAST
      mainfunc = getMainFunc propagated
  in case mainfunc of 
    Nothing -> throw @"perr" (NoMain moduleAST)
    Just e -> fullyReduce e
  where 

    reduceExpression :: Expression t p (i, Maybe (IdentifierMeaning t p i)) -> m (Expression t p (i, Maybe (IdentifierMeaning t p i)))
    reduceExpression (ParenthesizedExpression e _ _ ) = pure e
    reduceExpression e@(FunctionApplicationExpression func' args' _ _) = do
        func <- fullyReduce func'
        args <- mapM fullyReduce args'
        applyFunction func args
      where
        getIdentifierName = fst . getIdentifier
        applyFunction :: (Expression t p (i, Maybe (IdentifierMeaning t p i))) -> [Expression t p (i, Maybe (IdentifierMeaning t p i))] -> m (Expression t p (i, Maybe (IdentifierMeaning t p i)))
        applyFunction (FunctionLiteralExpression argNames expression _ _) args = -- TODO: Make this capture-avoidant by adding renaming
          if length idents /= length args
            then throw @"perr" $ IncorrectNumArgumentsProvided (depropagateExpression e)
            else pure $ foldr (\(b, a) e -> substitute e b a) expression zippedArgs
          where
            zippedArgs = zip (map getIdentifierName argNames) args
            idents = map getIdentifierName argNames
            substitute :: Expression t p (i, Maybe (IdentifierMeaning t p i)) -> i -> Expression t p (i, Maybe (IdentifierMeaning t p i)) -> Expression t p (i, Maybe (IdentifierMeaning t p i)) 
            substitute (ParenthesizedExpression e t p) before after = ParenthesizedExpression (substitute e before after) t p 
            substitute e@(NumberLiteral _ _ _) _ _ = e
            substitute e@(IdentifierExpression (LowercaseIdentifier (i, Just LambdaVar) p) t2 p2) before after 
              | i == before = after
              | otherwise   = e 
            substitute (FunctionLiteralExpression identifiers e t p) before after 
              | before `elem` (map getIdentifierName identifiers) = e 
              | any (\(LowercaseIdentifier (i, _) _) -> i `Set.member` (fv after)) identifiers = undefined "Not implemented yet"
              | otherwise                                         = FunctionLiteralExpression identifiers (substitute e before after) t p
            substitute (FunctionApplicationExpression funcToLookIn argsToLookIn t p) before after = FunctionApplicationExpression (substitute funcToLookIn before after) (map (\argToLookIn -> substitute argToLookIn before after) argsToLookIn) t p
            substitute (Annotation e1 e2 t p) before after = Annotation (substitute e1 before after) (substitute e2 before after) t p
            
            fv (ParenthesizedExpression e t p) = fv e
            fv (NumberLiteral _ _ _) = Set.empty 
            fv (IdentifierExpression (LowercaseIdentifier (i, _) _) t p ) = Set.singleton i
            fv (FunctionLiteralExpression idents e t p ) = foldr (flip Set.difference) (fv e) (map (Set.singleton . getIdentifierName) idents)
            fv (FunctionApplicationExpression f as t p ) = foldr Set.union (fv f) (map fv as)

    
    reduceExpression (FunctionLiteralExpression args func' t p) = do
      func <- fullyReduce func'
      pure $ FunctionLiteralExpression args func t p
    reduceExpression (Annotation e _ t p) = reduceExpression e
    reduceExpression   (IdentifierExpression (LowercaseIdentifier (i, Just (Ref e)) _) t p) = pure e 
    reduceExpression e@(IdentifierExpression (LowercaseIdentifier (i, _)            _) t p) = pure e 
    reduceExpression n@(NumberLiteral _ _ _) = pure n

    fullyReduce e = do 
      reduced <- reduceExpression e
      if reduced == e
        then pure e
        else fullyReduce (reduced)




example :: String
example = "module i `test module` \n\
          \z    = (-3.5)          \n\
          \func = (|x| x) : Number -> Number    \n\
          \main = z.func          \n\
          \"


