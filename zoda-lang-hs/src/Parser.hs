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

import Debug.Trace

parseModule :: (HasThrow "perr" (ProductionError p) m) => String -> m (Module SourcePosition)
parseModule text = handleResult result
  where 
    result = Data.Bifunctor.first (ZodaSyntaxError) (runParser (evalStateT moduleP (ParserState 0)) "book.md" text)
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
  sourcePosWrapper
    $   ( do
          numb <- try numberLiteralP
          pure (NumberLiteralExpression numb)
        )
    <|> ( do
          ident <- try lowercaseIdentifierP
          pure (IdentifierExpression ident)
        )

numberLiteralP :: ASTParser NumberLiteral
numberLiteralP = sourcePosWrapper $ do
  sign   <- try (string "-" *> pure False) <|> (pure True)
  majorS <- some (digitChar)
  minorS <- (try (char '.') *> some (digitChar)) <|> (pure "0")
  guard $ headUnsafe majorS /= '0'
  case readMay minorS of
    Just (minor) -> case (readMay (majorS <> "% 1") :: Maybe Rational) of
      Just (major) -> do
        let value1 = minor % (10 ^ (length minorS)) + (major) -- % is division, not modulo
            value2 = if sign then value1 else (negate value1)
        pure (NumberLiteral value2)
      _ -> empty
    _ -> empty
  where headUnsafe (x:_) = x

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
lowercaseIdentifierP = sourcePosWrapper $ do
  c    <- lowerChar
  rest <- many identifierCharacter
  pure . LowercaseIdentifier . fromString $ (c : rest)


identifierCharacter :: Parser Char
identifierCharacter = try letterChar <|> try alphaNumChar <|> try (char '\'') <|> try (char '-')


data ParserState = ParserState Int deriving (Show, Read, Eq, Ord)
type Parser a = StateT ParserState (Parsec Void String) a
type ASTParser a = Parser (a SourcePosition)
data SourcePosition = SourcePosition {_filePath :: String, _sourceLineStart :: Int, _sourceColumnStart  :: Int, _sourceLineEnd :: Int, _sourceColumnEnd  :: Int} deriving (Read, Eq, Ord)
instance Show SourcePosition where
  show (SourcePosition f l1 c1 l2 c2) = ""--"(SourcePosition " <> f <> " " <> (show l1) <> " " <> (show c1) <> " " <> (show l2) <> " " <> (show c2) <> ")"



sourcePosWrapper :: Parser (SourcePosition -> a) -> Parser a
sourcePosWrapper f = do
  (SourcePos n l1 c1) <- getSourcePos
  toApply             <- f
  (SourcePos _ l2 c2) <- getSourcePos
  pure $ toApply (SourcePosition n (unPos l1) (unPos c1) (unPos l2) (unPos c2))

sourcePosWrapperWithNewlines :: Parser (SourcePosition -> a) -> Parser a
sourcePosWrapperWithNewlines f = sourcePosWrapper f <* (some newline *> pure () <|> eof)



getRight :: Either (ParseErrorBundle String Void) b -> b
getRight (Right b  ) = b
getRight (Left  err) = error $ errorBundlePretty err

parseSomething :: Stream s => s -> StateT ParserState (Parsec e s) a -> Either (ParseErrorBundle s e) a
parseSomething text parser = (runParser (evalStateT parser (ParserState 0)) "no_file" text)


noNewlineOrChars :: (MonadParsec e s m, Token s ~ Char) => [Char] -> m (Token s)
noNewlineOrChars c = noneOf ('\n' : c)