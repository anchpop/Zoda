module Interpreter where
import Control.Monad
import Data.Maybe
import Ast
import qualified Data.Map as Map

data TypecheckingError p = ValueRedeclaration (Declaration p) deriving (Show, Read, Eq, Ord)

interpret (Module header declarations _) = valueMap
 where
  --valueMap :: Either (TypecheckingError p) (Map.Map String (Expression a))
  valueMap = foldM addValueToValuemap Map.empty declarations
  --addValueToValuemap
    -- :: (Map.Map String (Expression a)) -> Declaration a -> Either (TypecheckingError a) (Map.Map String (Expression a))
  addValueToValuemap m declaration@(Declaration (LowercaseIdentifier name _) expression position) =
    if isJust (Map.lookup name m) then Left (ValueRedeclaration (declaration)) else pure (Map.insert name expression m)
  assignee = undefined

example = "module i `test module`\n\
\test = 3\n\
\rend = test"