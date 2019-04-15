module Errors where
import Ast
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

data ProductionError p = ZodaSyntaxError (ParseErrorBundle String Void) | ValueRedeclaration (Declaration p) | UndeclaredValueReferenced (LowercaseIdentifier p) deriving (Show, Eq)