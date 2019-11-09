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

import Nominal hiding ((.))

parseModule :: String -> Either (ProductionError Untyped p () Text) (Module Untyped SourcePosition () Text)
parseModule text = handleResult result
 where
  result = Data.Bifunctor.first ZodaSyntaxError (runParser (evalStateT moduleP []) "module" text)
  handleResult (Left  e) = Left e
  handleResult (Right r) = pure r

moduleP :: Parser (Module Untyped SourcePosition () Text)
moduleP = sourcePosWrapperWithNewlines $ do
  header       <- moduleHeaderP
  declarations <- many declarationP
  pure (Module header declarations)


moduleHeaderP :: Parser (ModuleHeader Untyped SourcePosition () Text)
moduleHeaderP = sourcePosWrapperWithNewlines $ do
  string "module"
  some separatorChar
  (ident, _) <- sourcePosWrapper identifierP
  some separatorChar
  doc <- sourcePosWrapper tinydocP
  pure (ModuleHeader ident doc)


declarationP :: Parser (Declaration Untyped SourcePosition () Text)
declarationP = sourcePosWrapperWithNewlines $ do
  (ident, _) <- sourcePosWrapper identifierP
  some separatorChar
  char '='
  some separatorChar
  expression <- expressionP
  pure (Declaration ident expression)


padded s = many separatorChar *> s <* many separatorChar




expressionP :: Parser (Expression Untyped SourcePosition () Text)
expressionP = expParser --addition basicP <|> funcApp basicP <|> tarrow2 basicP <|> tarrow1 basicP <|> annotation basicP <|> basicP
  where 
    expParser = try $ makeExprParser (leftRec basicP leftRecP) 
                        [
                          [InfixL . try $ do 
                            padded $ string "."
                            pure (\expr1 expr2 -> FunctionApplicationExpression expr2 [expr1] Untyped (combineSourcePos expr1 expr2))
                          ],
                          [InfixR . try $ do 
                            padded $ string "+"
                            pure (\expr1 expr2 -> AddExpression expr1 expr2 Untyped (combineSourcePos expr1 expr2))
                          ],
                          [InfixR . try $ do
                            padded $ string "->"
                            pure (\expr1 expr2 -> TArrowNonbinding expr1 expr2 Untyped (combineSourcePos expr1 expr2))
                          ]
                        ]
                        
    basicP = foldl' (<|>) empty basicParsers
    leftRecP = foldl' (<|>) empty leftRecParsers
    basicParsers = [functionLiteralP, numbP, parenthesizedExpression, identifierExpP, tarrow2]
    leftRecParsers = [funcApp, annotation]
    parenthesizedExpression = try $ do
      char '('
      expression <- padded expressionP
      char ')'
      pure (ParenthesizedExpression expression Untyped)
    numbP = try $ do
      numb <- numberLiteralP
      pure (NumberLiteral (numerator numb) (denominator numb) Untyped)
    tarrow2 = try $ do
      (name, namepos) <- sourcePosWrapper identifierP
      let atom = with_fresh_named (unpack name) $ \(x :: Atom) -> x
      many separatorChar
      string ":"
      expr1 <- padded expressionP
      padded $ string "->"
      expr2 <- (withEnvInState [(name, (atom, namepos))] expressionP)
      pure (TArrowBinding expr1 ((name, (atom, namepos)) :. expr2) Untyped) 
    

    -- these parsers are left recursive, meaning they start with trying to return an expression and return an expression
    annotation = try $ do
      padded $ string ":"
      expr2 <- expressionP
      pure (\expr1 -> Annotation expr1 expr2 Untyped) 
    tarrow1 = try $ do
      padded $ string "->"
      expr2 <- expressionP
      pure (\expr1 -> TArrowNonbinding expr1 expr2 Untyped)
    funcApp = try $ typedotexp
      where
        typedotexp = do
          padded $ string "."
          f <- padded $ sourcePosWrapper basicP --expressionP
           
          string "("
          furtherArgs <- padded $ expressionP `sepBy` (many separatorChar *> string "," *> many separatorChar )
          string ")"

          pure $ \applicant -> FunctionApplicationExpression f (applicant:furtherArgs) Untyped  
  

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
          guard $ NonEmpty.head majorS /= '0'
          let major :: Rational
              major = justUnsafe (readMay ((toList majorS) <> " % 1"))
          pure $ if sign then major else negate major
        )
  where justUnsafe (Just x) = x

functionLiteralP :: ASTParser Expression
functionLiteralP = do
  char '|'
  identifierInfo <- padded $ (sourcePosWrapper identifierP) `sepBy1` (padded $ char ',')
  char '|'
  let identifiers = map fst identifierInfo
      duplicates = List.length (Set.fromList identifiers) < List.length identifiers
  when duplicates (customFailure DuplicateFunctionArgumentNames)
  let binders = map (\(name, pos) -> with_fresh_named (unpack name) (\(x :: Atom) -> (name, (x, pos)))) identifierInfo
  exp <- (withEnvInState binders expressionP)
  pure $ FunctionLiteralExpression (binders :. exp) Untyped


getEnv = do
  s <- get
  pure (join s)

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
                      Just (fresh_name, _) -> pure $ LambdaVariable (ident, fresh_name) Untyped 
                      Nothing         -> pure $ ReferenceVariable ident () Untyped


identifierCharacter :: Parser Char
identifierCharacter = try letterChar <|> try alphaNumChar <|> try (char '\'') <|> try (char '-')

type ParserState = [[(Text, (Atom, SourcePosition))]]
type Parser a = StateT ParserState (Parsec ZodaParseError String) a
type ASTParser a = Parser (SourcePosition -> a Untyped SourcePosition () Text)
data SourcePosition = SourcePosition {_filePath :: String, _sourceLineStart :: Int, _sourceColumnStart  :: Int, _sourceLineEnd :: Int, _sourceColumnEnd  :: Int} deriving (Read, Eq, NominalSupport, NominalShow, Generic, Nominal, Bindable)
instance Show SourcePosition where
  show (SourcePosition f l1 c1 l2 c2) = ""--"(SourcePosition \"" <> f <> "\" " <> (show l1) <> " " <> (show c1) <> " " <> (show l2) <> " " <> (show c2) <> ")"

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
  first <- p
  rest  <- many p
  pure $ first NonEmpty.:| rest

leftRec :: forall a . Show a => Parser (SourcePosition -> a) -> Parser (a -> (SourcePosition -> a)) -> Parser a
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
getRightZSE (Right b  ) = b
getRightZSE (Left  (ZodaSyntaxError err)) = error $ errorBundlePretty err

justUnsafe (Just a) = a

--parseSomething :: Stream s => s -> StateT ParserState (Parsec e s) a -> Either (ParseErrorBundle s e) a

parseSomething text parser = (runParser (evalStateT (parser <* eof) []) "no_file" text)

getSourcePosFromExpression (ParenthesizedExpression _ _ p ) = p  
getSourcePosFromExpression (NumberLiteral _ _ _ p ) = p 
getSourcePosFromExpression (AddExpression _ _ _ p ) = p 
getSourcePosFromExpression (ReferenceVariable _ _ _ p ) = p 
getSourcePosFromExpression (LambdaVariable _ _ p ) = p 
getSourcePosFromExpression (FunctionLiteralExpression _ _ p ) = p 
getSourcePosFromExpression (FunctionApplicationExpression _ _ _ p ) = p 
getSourcePosFromExpression (TArrowNonbinding _ _ _ p ) = p 
getSourcePosFromExpression (TArrowBinding _ _ _ p ) = p 
getSourcePosFromExpression (Annotation _ _ _ p ) = p 

combineSourcePos expr1 expr2 = SourcePosition filePath sourceLineStart sourceColumnStart sourceLineEnd sourceColumnEnd
  where 
    SourcePosition filePath sourceLineStart sourceColumnStart _ _ = getSourcePosFromExpression expr1
    SourcePosition _ _ _ sourceLineEnd sourceColumnEnd            = getSourcePosFromExpression expr2

noNewlineOrChars :: (MonadParsec e s m, Token s ~ Char) => [Char] -> m (Token s)
noNewlineOrChars c = noneOf ('\n' : c)