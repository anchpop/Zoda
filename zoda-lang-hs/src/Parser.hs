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
  evalState (runParserT moduleP "book.md" text) (ParserState 0)

moduleP = empty


parseTesting :: String -> Either (ParseErrorBundle String Void) ParserState
parseTesting text =
  (runParser (evalStateT testP (ParserState 0)) "book.md" text)

testP :: SParser ParserState--testP :: ParserM m => m Char
testP = do
  (try (oneP)) <|> (try (char '2'))
  g <- get
  pure g

oneP :: SParser Char--oneP :: ParserM m => m Char
oneP = do
  (ParserState i) <- get
  put (ParserState (i + 1))
  char '1'


type ParserM m = (MonadParsec Void String m, MonadState ParserState m)
data ParserState = ParserState Int deriving (Show, Read, Eq)
type Parser = Parsec Void String
type SParser a = StateT ParserState Parser a
data SourcePosition = SourcePosition {_filePath :: String, _sourceLineStart :: Int, _sourceColumnStart  :: Int, _sourceLineEnd :: Int, _sourceColumnEnd  :: Int} deriving (Show, Read, Eq)
