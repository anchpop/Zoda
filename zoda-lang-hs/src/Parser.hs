module Parser where
import Ast

import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Control.Monad.State

import Debug.Trace

parseModule
  :: String -> Either (ParseErrorBundle String Void) (Module SourcePosition)
parseModule text =
  runParser (evalStateT moduleP (ParserState 0)) "book.md" text

moduleP :: ASTParser Module
moduleP = empty

lowercaseIdentifierP :: ASTParser Identifier
lowercaseIdentifierP = sourcePosWrapper $ do
  c    <- lowerChar
  rest <- many identifierCharacter
  pure (Identifier (c : rest))



identifierCharacter :: Parser Char
identifierCharacter =
  try letterChar <|> try alphaNumChar <|> try (char '\'') <|> try (char '-')

sourcePosWrapper :: Parser (SourcePosition -> a) -> Parser a
sourcePosWrapper f = do
  (SourcePos n l1 c1) <- getSourcePos
  toApply             <- f
  (SourcePos _ l2 c2) <- getSourcePos
  pure $ toApply (SourcePosition n (unPos l1) (unPos c1) (unPos l2) (unPos c2))


data ParserState = ParserState Int deriving (Show, Read, Eq)
type Parser a = StateT ParserState (Parsec Void String) a
type ASTParser a = Parser (a SourcePosition)
data SourcePosition = SourcePosition {_filePath :: String, _sourceLineStart :: Int, _sourceColumnStart  :: Int, _sourceLineEnd :: Int, _sourceColumnEnd  :: Int} deriving (Show, Read, Eq)



getRight :: Either (ParseErrorBundle String Void) b -> b
getRight (Right b  ) = b
getRight (Left  err) = error $ errorBundlePretty err


parseSomething :: String -> Parser (a SourcePosition) -> a SourcePosition
parseSomething text parser =
  getRight (runParser (evalStateT parser (ParserState 0)) "book.md" text)
