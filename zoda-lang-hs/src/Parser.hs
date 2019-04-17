module Parser where
import ClassyPrelude hiding (try, many, some)
import Ast
import Basic

import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Control.Monad.State
import Capability.Error

import Data.Ratio
import qualified Data.Bifunctor as Data.Bifunctor

import Text.Megaparsec.Debug 

parseModule :: (HasThrow "perr" (ProductionError p) m) => String -> m (Module SourcePosition)
parseModule text = handleResult result
  where 
    result = Data.Bifunctor.first (ZodaSyntaxError) (runParser (evalStateT moduleP (ParserState 0)) "module" text)
    handleResult (Left e) = throw @"perr" e
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
  ident <- lowercaseIdentifierP
  some separatorChar
  doc <- tinydocP
  pure (ModuleHeader ident doc)


declarationP :: ASTParser Declaration
declarationP = sourcePosWrapperWithNewlines $ do
  ident <- lowercaseIdentifierP
  some separatorChar
  char '='
  some separatorChar
  expression <- expressionP
  pure (Declaration ident expression)


expressionP :: ASTParser Expression
expressionP = 
        funcAppOnParenthesized
    <|> funcAppOnNum
    <|> funcAppOnFliteral
    <|> funcAppOnIdent
    <|> parenthesizedExpression
    <|> numb
    <|> ident
    <|> fliteral
  where 
    funcAppOnParenthesized = funcAppWrapper parenthesizedExpression
    funcAppOnFliteral = funcAppWrapper fliteral
    funcAppOnIdent = funcAppWrapper ident
    funcAppOnNum = funcAppWrapper numb
    funcAppWrapper expType = sourcePosWrapper . try . (fmap (uncurry FunctionApplicationExpression)) $  typedotexp
      where
        typedotexp = do 
          applicant <- expType
          string "."
          f <- expressionP
          pure (f, [applicant])
    parenthesizedExpression = sourcePosWrapper . try $ do 
      char '('
      many separatorChar
      exp <- expressionP
      many separatorChar
      char ')'
      pure (ParenthesizedExpression exp)
      
    numb     = sourcePosWrapper . try $ do
      numb <- numberLiteralP
      pure (NumberLiteralExpression numb)
    ident    = sourcePosWrapper . try $ do
      ident <- lowercaseIdentifierP
      pure (IdentifierExpression ident)
    fliteral = sourcePosWrapper . try $ do
      flit <- functionLiteralP
      pure (FunctionLiteralExpression flit)

numberLiteralP :: ASTParser NumberLiteral
numberLiteralP = sourcePosWrapper $ 
  try (do
    sign   <- try (string "-" *> pure False) <|> (pure True)
    majorS <- some (digitChar)
    minorS <- (try (char '.') *> some (digitChar)) <|> (pure "0")
    guard $ headUnsafe majorS /= '0'
    case readMay minorS of
      Just (minor) -> case (readMay (majorS <> " % 1") :: Maybe Rational) of
        Just (major) -> do
          let value1 = minor % (10 ^ (length minorS)) + (major) -- % is division, not modulo
              value2 = if sign then value1 else (negate value1)
          pure (NumberLiteral value2)
        _ -> empty
      _ -> empty)
  <|> (do 
    sign   <- try (string "-" *> pure False) <|> (pure True)
    majorS <- some (digitChar)
    guard $ headUnsafe majorS /= '0'
    let 
      major :: Rational
      major = justUnsafe (readMay (majorS <> " % 1"))
    pure . NumberLiteral $ if sign then major else negate major
    )
  where 
    headUnsafe (x:_) = x
    justUnsafe (Just x) = x

functionLiteralP :: ASTParser FunctionLiteral
functionLiteralP = sourcePosWrapper $ do
  char '|'
  identifiers <- lowercaseIdentifierP `sepBy1` (char ',' *> some separatorChar)
  char '|'
  some separatorChar
  string "->"
  some separatorChar
  exp <- expressionP
  pure $ FunctionLiteral identifiers exp


tinydocP :: ASTParser Tinydoc
tinydocP = sourcePosWrapper $ do
  char '`'
  doc <- some (noNewlineOrChars "`")
  char '`'
  pure . Tinydoc . fromString $ doc


uppercaseIdentifierP :: ASTParser UppercaseIdentifier
uppercaseIdentifierP = sourcePosWrapper $ do
  c    <- upperChar
  rest <- many identifierCharacter
  pure . UppercaseIdentifier . fromString $ (c : rest)


lowercaseIdentifierP :: ASTParser LowercaseIdentifier
lowercaseIdentifierP = sourcePosWrapper $ (do
  c    <- lowerChar
  rest <- many identifierCharacter
  pure . LowercaseIdentifier . fromString $ (c : rest))


identifierCharacter :: Parser Char
identifierCharacter = try letterChar <|> try alphaNumChar <|> try (char '\'') <|> try (char '-')


data ParserState = ParserState Int deriving (Show, Read, Eq, Ord)
type Parser a = StateT ParserState (Parsec Void String) a
type ASTParser a = Parser (a SourcePosition)
data SourcePosition = SourcePosition {_filePath :: String, _sourceLineStart :: Int, _sourceColumnStart  :: Int, _sourceLineEnd :: Int, _sourceColumnEnd  :: Int} deriving (Read, Eq, Ord)
instance Show SourcePosition where
  show (SourcePosition f l1 c1 l2 c2) = "(SourcePosition \"" <> f <> "\" " <> (show l1) <> " " <> (show c1) <> " " <> (show l2) <> " " <> (show c2) <> ")"



sourcePosWrapper :: Parser (SourcePosition -> a) -> Parser a
sourcePosWrapper f = do
  (SourcePos n l1 c1) <- getSourcePos
  toApply             <- f
  (SourcePos _ l2 c2) <- getSourcePos
  pure $ toApply (SourcePosition n (unPos l1) (unPos c1) (unPos l2) (unPos c2))

sourcePosWrapperWithNewlines :: Parser (SourcePosition -> a) -> Parser a
sourcePosWrapperWithNewlines f = sourcePosWrapper f <* (some newline *> pure () <|> (lookAhead eof))



getRight :: Either (ParseErrorBundle String Void) b -> b
getRight (Right b  ) = b
getRight (Left  err) = error $ errorBundlePretty err

justUnsafe (Just a) = a
expression' :: Parser Int
expression' = 
  (try $ do
    n1 <- number'
    char '+'
    n2 <- expression'
    pure (n1 + n2))
  <|> try number'
  
number' :: Parser Int
number' = do
  i <- some digitChar
  pure . justUnsafe . (readMay :: String -> Maybe Int) $ i

--parseSomething :: Stream s => s -> StateT ParserState (Parsec e s) a -> Either (ParseErrorBundle s e) a
parseSomething text parser = (runParser (evalStateT (parser <* eof) (ParserState 0)) "no_file" text)


noNewlineOrChars :: (MonadParsec e s m, Token s ~ Char) => [Char] -> m (Token s)
noNewlineOrChars c = noneOf ('\n' : c)