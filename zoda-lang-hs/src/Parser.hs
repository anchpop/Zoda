module Parser where
import Ast

import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Control.Monad.State

import Debug.Trace

parseModule :: String -> Either (ParseErrorBundle String Void) (Module SourcePosition)
parseModule text = runParser (evalStateT moduleP (ParserState 0)) "book.md" text

moduleP :: ASTParser Module
moduleP = empty



expressionP :: ASTParser Expression
expressionP = sourcePosWrapper $ do
  numb <- try numberLiteralP
  pure (Expression numb)

numberLiteralP :: ASTParser NumberLiteral
numberLiteralP = sourcePosWrapper $ do
  sign  <- try (char '-' *> pure False) <|> (pure True)
  major <- some (digitChar)
  minor <- (try (char '.') *> some (digitChar)) <|> (pure "0")
  pure (NumberLiteral sign (read major) (read minor))

tinydocP :: ASTParser Tinydoc
tinydocP = sourcePosWrapper $ do
  char '`'
  doc <- some (noNewlineOrChars "`")
  char '`'
  pure (Tinydoc doc)


uppercaseIdentifierP :: ASTParser UppercaseIdentifier
uppercaseIdentifierP = sourcePosWrapper $ do
  c    <- upperChar
  rest <- many identifierCharacter
  pure (UppercaseIdentifier (c : rest))


lowercaseIdentifierP :: ASTParser LowercaseIdentifier
lowercaseIdentifierP = sourcePosWrapper $ do
  c    <- lowerChar
  rest <- many identifierCharacter
  pure (LowercaseIdentifier (c : rest))


identifierCharacter :: Parser Char
identifierCharacter = try letterChar <|> try alphaNumChar <|> try (char '\'') <|> try (char '-')


data ParserState = ParserState Int deriving (Show, Read, Eq)
type Parser a = StateT ParserState (Parsec Void String) a
type ASTParser a = Parser (a SourcePosition)
data SourcePosition = SourcePosition {_filePath :: String, _sourceLineStart :: Int, _sourceColumnStart  :: Int, _sourceLineEnd :: Int, _sourceColumnEnd  :: Int} deriving (Show, Read, Eq)



sourcePosWrapper :: Parser (SourcePosition -> a) -> Parser a
sourcePosWrapper f = do
  (SourcePos n l1 c1) <- getSourcePos
  toApply             <- f
  (SourcePos _ l2 c2) <- getSourcePos
  pure $ toApply (SourcePosition n (unPos l1) (unPos c1) (unPos l2) (unPos c2))

getRight :: Either (ParseErrorBundle String Void) b -> b
getRight (Right b  ) = b
getRight (Left  err) = error $ errorBundlePretty err

-- parseSomething :: String -> StateT ParserState (Parsec Void String) b -> b
parseSomething text parser = (runParser (evalStateT parser (ParserState 0)) "no_file" text)


noNewlineOrChars :: (MonadParsec e s m, Token s ~ Char) => [Char] -> m (Token s)
noNewlineOrChars c = noneOf ('\n' : c)
