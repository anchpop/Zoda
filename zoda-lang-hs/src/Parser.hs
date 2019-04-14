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
  (runParser (evalStateT moduleP (ParserState 0)) "book.md" text)

moduleP :: ASTParser Module
moduleP = empty



data ParserState = ParserState Int deriving (Show, Read, Eq)
type Parser a = StateT ParserState (Parsec Void String) a
type ASTParser a = Parser (a SourcePosition)
data SourcePosition = SourcePosition {_filePath :: String, _sourceLineStart :: Int, _sourceColumnStart  :: Int, _sourceLineEnd :: Int, _sourceColumnEnd  :: Int} deriving (Show, Read, Eq)
