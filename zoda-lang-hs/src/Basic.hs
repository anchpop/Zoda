module Basic where
import ClassyPrelude
import Ast
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Capability.Error
import Capability.Writer
import CopyPropagatedProgram
import Control.Monad.Except (ExceptT (..), Except)



data ProductionError i p = ZodaSyntaxError (ParseErrorBundle String Void) | ValueRedeclaration (Declaration i p) | UndeclaredValueReferenced (LowercaseIdentifier i p) | NoMain (Module i p) 
  deriving (Show, Eq)
  deriving anyclass Exception

newtype M i p r = M { runM :: Either (ProductionError i p) r }
  deriving (Functor, Applicative, Monad) via Either (ProductionError i p)
  deriving (HasThrow "perr" (ProductionError i p)) via
    MonadError (Except (ProductionError i p))
    
  --deriving (HasThrow "perr" (ProductionError p)) via
    --MonadError (Except (ProductionError p))

    