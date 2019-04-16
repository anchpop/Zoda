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



data ProductionError p = ZodaSyntaxError (ParseErrorBundle String Void) | ValueRedeclaration (Declaration p) | UndeclaredValueReferenced (LowercaseIdentifier p) | NoMain (Module p) 
  deriving (Show, Eq)
  deriving anyclass Exception

newtype M p r = M { runM :: Either (ProductionError p) r }
  deriving (Functor, Applicative, Monad) via Either (ProductionError p)
  deriving (HasThrow "perr" (ProductionError p)) via
    MonadError (Except (ProductionError p))
    
  --deriving (HasThrow "perr" (ProductionError p)) via
    --MonadError (Except (ProductionError p))

    