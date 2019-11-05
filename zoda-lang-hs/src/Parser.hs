module Parser where
import ClassyPrelude hiding (try, many)
import Ast
import Basic

import Text.Megaparsec hiding (State, some)
import Text.Megaparsec.Char
import Control.Monad.State
import Capability.Error

import qualified Data.List.NonEmpty as NonEmpty
import Data.Ratio
import qualified Data.Bifunctor as Data.Bifunctor
import qualified Data.Set as Set
import qualified Data.List as List

import Text.Megaparsec.Debug
import Nominal hiding ((.))

parseModule :: (HasThrow "perr" (ProductionError Untyped p () Text) m) => String -> m (Module Untyped SourcePosition () Text)
parseModule text = handleResult result
 where
  result = Data.Bifunctor.first ZodaSyntaxError (runParser (evalStateT moduleP []) "module" text)
  handleResult (Left  e) = throw @"perr" e
  handleResult (Right r) = pure r

moduleP :: ASTParser Module
moduleP = sourcePosWrapper $ do
  header       <- moduleHeaderP
  declarations <- many declarationP
  pure (Module header declarations)


moduleHeaderP :: ASTParser ModuleHeader
moduleHeaderP = sourcePosWrapperWithNewlines $ do
  string "module"
  some separatorChar
  (ident, _) <- identifierP
  some separatorChar
  doc <- tinydocP
  pure (ModuleHeader ident doc)


declarationP :: ASTParser Declaration
declarationP = sourcePosWrapperWithNewlines $ do
  (ident, _) <- identifierP
  some separatorChar
  char '='
  some separatorChar
  expression <- expressionP
  pure (Declaration ident expression)


expressionP :: ASTParser Expression
expressionP = funcApp basicP <|> tarrow2 basicP <|> tarrow1 basicP <|> annotation basicP <|> basicP
  where 
    basicP = foldl' (<|>) empty basicParsers
    basicParsers = [functionLiteralP, numbP, parenthesizedExpression, identifierExpP]
    parenthesizedExpression = sourcePosWrapper . try $ do
      char '('
      many separatorChar
      expression <- expressionP
      many separatorChar
      char ')'
      pure (ParenthesizedExpression expression Untyped)
    numbP = sourcePosWrapper . try $ do
      numb <- numberLiteralP
      pure (NumberLiteral (numerator numb) (denominator numb) Untyped)

    -- these parsers are left recursive, meaning they start with trying to return an expression and return an expression
    annotation expType = sourcePosWrapper . try $ do
      expr1 <- expType
      many separatorChar
      string ":"
      many separatorChar
      expr2 <- expressionP
      pure (Annotation expr1 expr2 Untyped) 
    tarrow1 expType = sourcePosWrapper . try $ do
      expr1 <- expType
      many separatorChar
      string "->"
      many separatorChar
      expr2 <- expressionP
      pure (TArrowNonbinding expr1 expr2 Untyped)
    tarrow2 expType = sourcePosWrapper . try $ do
      (name, namepos) <- identifierP
      let atom = with_fresh_named (unpack name) $ \(x :: Atom) -> x
      many separatorChar
      string ":"
      many separatorChar
      expr1 <- expType
      many separatorChar
      string "->"
      many separatorChar
      expr2 <- withEnvInState [] expressionP
      pure (TArrowBinding expr1 ((name, (atom, namepos)) :. expr2) Untyped) 
    funcApp expType = sourcePosWrapper . try $ typedotexp
      where
        typedotexp = do
          applicant <- expType
          many separatorChar
          string "."
          many separatorChar
          f <- expressionP
          many separatorChar
          moreApplicants <- (try $ do 
            string "("
            many separatorChar
            furtherArgs <- expressionP `sepBy` (many separatorChar *> string "," *> many separatorChar ) 
            many separatorChar
            string ")"
            pure furtherArgs
            ) <|> pure []
          pure $ FunctionApplicationExpression f (applicant:moreApplicants) Untyped  
  

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
functionLiteralP = sourcePosWrapper $ do
  char '|'
  identifierInfo <- identifierP `sepBy1` (many separatorChar *> char ',' *> many separatorChar)
  char '|'
  let identifiers = map fst identifierInfo
      duplicates = List.length (Set.fromList identifiers) < List.length identifiers
  when duplicates (customFailure DuplicateFunctionArgumentNames)
  let binders = map (\(name, pos) -> with_fresh_named (unpack name) (\(x :: Atom) -> (name, (x, pos)))) identifierInfo
  some separatorChar


  -- string "->"
  -- some separatorChar
  exp <- withEnvInState binders expressionP
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
tinydocP = sourcePosWrapper $ do
  char '`'
  doc <- some (noNewlineOrChars "`")
  char '`'
  pure . Tinydoc . fromString $ doc


identifierP :: Parser (Text, SourcePosition) 
identifierP = sourcePosWrapper $ do
                c    <- letterChar 
                rest <- many identifierCharacter
                let ident = c:rest
                pure $ (\x -> (fromString ident, x))

identifierExpP :: ASTParser Expression
identifierExpP = sourcePosWrapper
              ( do
                  (ident, _) <- identifierP

                  env  <- getEnv
                  case ident `lookup` env of
                    Just (fresh_name, _) -> pure $ LambdaVariable (ident, fresh_name) Untyped 
                    Nothing         -> pure $ ReferenceVariable ident () Untyped
                )


identifierCharacter :: Parser Char
identifierCharacter = try letterChar <|> try alphaNumChar <|> try (char '\'') <|> try (char '-')

type ParserState = [[(Text, (Atom, SourcePosition))]]
type Parser a = StateT ParserState (Parsec ZodaParseError String) a
type ASTParser a = Parser (a Untyped SourcePosition () Text)
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


getRight :: Either (ParseErrorBundle String ZodaParseError) b -> b
getRight (Right b  ) = b
getRight (Left  err) = error $ errorBundlePretty err
getRightZSE (Right b  ) = b
getRightZSE (Left  (ZodaSyntaxError err)) = error $ errorBundlePretty err

justUnsafe (Just a) = a

--parseSomething :: Stream s => s -> StateT ParserState (Parsec e s) a -> Either (ParseErrorBundle s e) a

parseSomething text parser = (runParser (evalStateT (parser <* eof) []) "no_file" text)


noNewlineOrChars :: (MonadParsec e s m, Token s ~ Char) => [Char] -> m (Token s)
noNewlineOrChars c = noneOf ('\n' : c)