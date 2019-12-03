module Parser where
import ClassyPrelude hiding (try, many)
import Ast
import Basic

import Text.Megaparsec hiding (State, some)
import Text.Megaparsec.Char
import Control.Monad.State

import qualified Data.List.NonEmpty as NonEmpty
import Data.Ratio
import qualified Data.Bifunctor as Data.Bifunctor
import qualified Data.Set as Set
import qualified Data.List as List
import Text.Megaparsec.Debug
import Control.Monad.Combinators.Expr
import qualified Data.Maybe as Maybe

import Nominal hiding ((.))

parseModule :: Text -> Either (ProductionError Untyped p (Text, SourcePosition) Text) (Module Untyped SourcePosition (Text, SourcePosition) Text)
parseModule text = handleResult result
 where
  result = Data.Bifunctor.first ZodaSyntaxError (runParser (evalStateT moduleP []) "module" (unpack text))
  handleResult (Left  e) = Left e
  handleResult (Right r) = pure r

moduleP :: Parser (Module Untyped SourcePosition (Text, SourcePosition) Text)
moduleP = sourcePosWrapperWithNewlines $ do
  header       <- moduleHeaderP
  declarations <- many declarationP
  pure (Module header declarations)


moduleHeaderP :: Parser (ModuleHeader Untyped SourcePosition (Text, SourcePosition) Text)
moduleHeaderP = sourcePosWrapperWithNewlines $ do
  string "module"
  some separatorChar
  (ident, _) <- sourcePosWrapper identifierP
  some separatorChar
  doc <- sourcePosWrapper tinydocP
  pure (ModuleHeader ident doc)


declarationP :: Parser (Declaration Untyped SourcePosition (Text, SourcePosition) Text)
declarationP = sourcePosWrapperWithNewlines $ do
  (ident, _) <- sourcePosWrapper identifierP
  some separatorChar
  char '='
  some separatorChar
  expression <- expressionP
  pure (Declaration ident expression)

padded :: (MonadParsec e s f, Token s ~ Char) => f a -> f a
padded s = many separatorChar *> s <* many separatorChar 


expressionP :: Parser (Expression Untyped SourcePosition (Text, SourcePosition) Text)
expressionP = expParser
  where 
    expParser = try $ makeExprParser (leftRec basicP leftRecP) 
                        [
                          [InfixL . try $ do 
                            padded $ string "."
                            pure (\expr1 expr2 -> FunctionApplicationExpression expr2 (expr1 NonEmpty.:| []) Untyped (combineSourcePos expr1 expr2))
                          ],
                          [InfixR . try $ do 
                            padded $ string "+"
                            pure (\expr1 expr2 -> AddExpression expr1 expr2 Untyped (combineSourcePos expr1 expr2))
                          ],
                          [
                            InfixR . try $ do 
                              padded $ string "::"
                              pure (\expr1 expr2 -> Annotation expr1 expr2 Untyped (combineSourcePos expr1 expr2))
                          ]
                        ]
                        
    basicP = foldl' (<|>) empty basicParsers
    leftRecP = foldl' (<|>) empty leftRecParsers
    basicParsers = [tarrow2, functionLiteralP, numbP, parenthesizedExpression, identifierExpP]
    leftRecParsers = [funcApp]
    parenthesizedExpression = try $ do
      char '('
      expression <- padded expressionP
      char ')'
      pure (ParenthesizedExpression expression Untyped)
    numbP = try $ do
      numb <- numberLiteralP
      pure (NumberLiteral numb Untyped)
    tarrow2 = try $ do
      char '('
      s <- get
      binders <- fmap (NonEmpty.fromList) $ (binder1P <|> binder2P) `sepBy1` (padded $ string ",")
      char ')'
      padded $ string "->"
      expr2 <- expressionP
      put s
      
      pure (TArrowBinding (makeTelescope binders expr2) Untyped) 
        where 
          makeTelescope ((a, e) NonEmpty.:| [])        expr2 = Pi e (a :. expr2)
          makeTelescope ((a, e) NonEmpty.:| (a1 : as)) expr2 = Scope e (a :. (makeTelescope (a1 NonEmpty.:| as) expr2))
          binder1P = try $ do 
            (name, namepos) <- padded $ sourcePosWrapper identifierP
            let atom = with_fresh_named (unpack name) id
            modify ([(name, (atom, namepos))]:)
            many separatorChar
            string ":"
            expr <- padded expressionP
            pure (Just (atom, NoBind name, NoBind namepos), expr)
          binder2P = try $ do 
            expr <- padded expressionP
            pure (Nothing, expr)
    

    -- these parsers are left recursive, meaning they start with trying to return an expression and return an expression
    funcApp = try $ typedotexp
      where
        typedotexp = do
          padded $ string "."
          f <- padded $ sourcePosWrapper basicP --expressionP
           
          string "("
          furtherArgs <- padded $ expressionP `sepBy` (many separatorChar *> string "," *> many separatorChar )
          string ")"

          pure $ \applicant -> FunctionApplicationExpression f (applicant NonEmpty.:| furtherArgs) Untyped  
  

numberLiteralP :: Parser Rational
numberLiteralP = try
          ( do
            sign   <- try (string "-" *> pure False) <|> (pure True)
            majorS <- some' (digitChar)
            minorS <- (try (char '.') *> some' (digitChar)) <|> (pure $ '0' NonEmpty.:| [])
            guard $ NonEmpty.head majorS /= '0'
            case readMay minorS of
              Just (minor) -> case (readMay ((toList majorS) <> " % 1") :: Maybe Rational) of
                Just (major) -> do
                  let value1 = minor % (10 ^ (length minorS)) + (major) -- % is division, not modulo

                      value2 = if sign then value1 else (negate value1)
                  pure (value2)
                _ -> empty
              _ -> empty
          )
    <|> ( do
          sign   <- try (string "-" *> pure False) <|> (pure True)
          majorS <- some' (digitChar)
          if majorS == '0' NonEmpty.:| [] 
            then pure 0
            else do
              guard $ NonEmpty.head majorS /= '0'
              let major :: Rational
                  major = justUnsafe (readMay ((toList majorS) <> " % 1"))
              pure $ if sign then major else negate major
        )
  where 
    justUnsafe (Just x) = x
    justUnsafe _        = error "shouldn't be possible"

functionLiteralP :: ASTParser Expression
functionLiteralP = do
  s <- get
  char '|'
  binders <- padded $ binderP `sepBy1` (padded $ char ',')
  char '|'
  some separatorChar
  let identifiers = map (\((_, name, _), _) -> name) binders
      duplicates = List.length (Set.fromList identifiers) < List.length identifiers
  when duplicates (customFailure DuplicateFunctionArgumentNames)
  express <- expressionP
  put s
  pure $ FunctionLiteralExpression (makeFlit (NonEmpty.fromList binders) express) Untyped
  where makeFlit :: NonEmpty.NonEmpty ((Atom, Text, SourcePosition), (Expression Untyped SourcePosition (Text, SourcePosition) Text)) -> Expression Untyped SourcePosition (Text, SourcePosition) Text -> FunctionLiteral Untyped SourcePosition (Text, SourcePosition) Text
        makeFlit (((a, i, p), e) NonEmpty.:| [])        expr2 = LastArg e ((a, NoBind i, NoBind p) :. expr2)
        makeFlit (((a, i, p), e) NonEmpty.:| (a1 : as)) expr2 = Arg     e ((a, NoBind i, NoBind p) :. (makeFlit (a1 NonEmpty.:| as) expr2))
                  
        binderP = do 
          (name, namepos) <- padded $ sourcePosWrapper identifierP
          let atom = with_fresh_named (unpack name) id
          modify ([(name, (atom, namepos))]:)
          many separatorChar
          string ":"
          expr <- padded expressionP
          pure ((atom, name, namepos), expr)

getEnv :: StateT ParserState (Parsec ZodaParseError String) [(Text, (Atom, SourcePosition))]
getEnv = do
  s <- get
  pure (join s)

withEnvInState :: MonadState [a] m => a -> m b -> m b
withEnvInState e a = do
  s <- get
  put (e:s) 
  val <- a
  put s
  pure val


tinydocP :: ASTParser Tinydoc
tinydocP = do
  char '`'
  doc <- some (noNewlineOrChars "`")
  char '`'
  pure . Tinydoc . fromString $ doc


identifierP :: Parser (SourcePosition -> (Text, SourcePosition)) 
identifierP = do
                c    <- letterChar 
                rest <- many identifierCharacter
                let ident = c:rest
                pure $ (\x -> (fromString ident, x))

identifierExpP :: ASTParser Expression
identifierExpP = do (ident, _) <- sourcePosWrapper identifierP
                    env  <- getEnv
                    case ident `lookup` env of
                      Just (atom, _) -> pure $ LambdaVariable (atom, ident) Untyped 
                      Nothing        -> pure $ (\s -> ReferenceVariable ident (ident, s) Untyped s)


identifierCharacter :: Parser Char
identifierCharacter = try letterChar <|> try alphaNumChar <|> try (char '\'') <|> try (char '-')

type ParserState = [[(Text, (Atom, SourcePosition))]]
type Parser a = StateT ParserState (Parsec ZodaParseError String) a
type ASTParser a = Parser (SourcePosition -> a Untyped SourcePosition (Text, SourcePosition) Text)
data SourcePosition = SourcePosition {_filePath :: String, _sourceLineStart :: Int, _sourceColumnStart  :: Int, _sourceLineEnd :: Int, _sourceColumnEnd  :: Int} 
                    | Base 
                    deriving (Read, Eq, NominalSupport, NominalShow, Generic, Nominal, Bindable)
instance Show SourcePosition where
  --show _ = ""
  show (SourcePosition f l1 c1 l2 c2) = "(SourcePosition \"" <> f <> "\" " <> (show l1) <> " " <> (show c1) <> " " <> (show l2) <> " " <> (show c2) <> ")"
  show (Base) = "Base" 

sourcePosWrapper :: Parser (SourcePosition -> a) -> Parser a
sourcePosWrapper f = do
  (SourcePos n l1 c1) <- getSourcePos
  toApply             <- f
  (SourcePos _ l2 c2) <- getSourcePos
  pure $ toApply (SourcePosition n (unPos l1) (unPos c1) (unPos l2) (unPos c2))

sourcePosWrapperWithNewlines :: Parser (SourcePosition -> a) -> Parser a
sourcePosWrapperWithNewlines f = sourcePosWrapper f <* (many separatorChar *> some newline *> pure () <|> (lookAhead eof))


some' :: Parser a -> Parser (NonEmpty.NonEmpty a)
some' p = do
  f <- p
  rest  <- many p
  pure $ f NonEmpty.:| rest

leftRec :: forall a . Parser (SourcePosition -> a) -> Parser (a -> (SourcePosition -> a)) -> Parser a
leftRec p op = do pos <- getSourcePos
                  initial <- sourcePosWrapper p
                  rest initial pos
  where
    rest :: a -> SourcePos -> Parser a
    rest x (SourcePos n l1 c1) = do f <- op
                                    (SourcePos _ l2 c2) <- getSourcePos
                                    rest (f x (SourcePosition n (unPos l1) (unPos c1) (unPos l2) (unPos c2))) (SourcePos n l1 c1)
          <|> pure x

getRight :: Either (ParseErrorBundle String ZodaParseError) b -> b
getRight (Right b  ) = b
getRight (Left  err) = error $ errorBundlePretty err

getRightZSE :: Either (ProductionError t p1 m i) p2 -> p2
getRightZSE (Right b  )                   = b
getRightZSE (Left  (ZodaSyntaxError err)) = error $ errorBundlePretty err
getRightZSE (Left TypeErr)                = error "type error!"
getRightZSE (Left  _)                     = error "Parse error!"


--parseSomething :: Stream s => s -> StateT ParserState (Parsec e s) a -> Either (ParseErrorBundle s e) a

parseSomething :: (Stream s, Ord e) => s -> StateT [a1] (ParsecT e s Identity) a2 -> Either (ParseErrorBundle s e) a2
parseSomething text parser = (runParser (evalStateT (parser <* eof) []) "no_file" text)

getSourcePosFromExpression :: Expression t p m i -> p
getSourcePosFromExpression (ParenthesizedExpression _ _ p ) = p  
getSourcePosFromExpression (NumberLiteral _ _ p ) = p 
getSourcePosFromExpression (AddExpression _ _ _ p ) = p 
getSourcePosFromExpression (ReferenceVariable _ _ _ p ) = p 
getSourcePosFromExpression (LambdaVariable _ _ p ) = p 
getSourcePosFromExpression (FunctionLiteralExpression _ _ p ) = p 
getSourcePosFromExpression (FunctionApplicationExpression _ _ _ p ) = p 
getSourcePosFromExpression (TArrowBinding _ _ p ) = p 
getSourcePosFromExpression (Annotation _ _ _ p ) = p 
getSourcePosFromExpression (FirstExpression _ _ p ) = p  
getSourcePosFromExpression (SecondExpression _ _ p) = p
getSourcePosFromExpression (PairExpression _ _ _ p) = p
getSourcePosFromExpression (TSigmaBinding _ _ _ p) = p
getSourcePosFromExpression (UniverseExpression _ _ p) = p
getSourcePosFromExpression (NatTypeExpression _ p) = p

combineSourcePos :: Expression t1 SourcePosition m1 i1 -> Expression t2 SourcePosition m2 i2 -> SourcePosition
combineSourcePos expr1 expr2 = SourcePosition filePath sourceLineStart sourceColumnStart sourceLineEnd sourceColumnEnd
  where 
    SourcePosition filePath sourceLineStart sourceColumnStart _ _ = getSourcePosFromExpression expr1
    SourcePosition _ _ _ sourceLineEnd sourceColumnEnd            = getSourcePosFromExpression expr2

noNewlineOrChars :: (MonadParsec e s m, Token s ~ Char) => [Char] -> m (Token s)
noNewlineOrChars c = noneOf ('\n' : c)



