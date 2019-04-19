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



data ProductionError p i = ZodaSyntaxError (ParseErrorBundle String Void) | ValueRedeclaration (Declaration p i) | UndeclaredValueReferenced (LowercaseIdentifier p i) | NoMain (Module p i) 
  deriving (Show, Eq)
  deriving anyclass Exception

newtype M p i r = M { runM :: Either (ProductionError p i) r }
  deriving (Functor, Applicative, Monad) via Either (ProductionError p i)
  deriving (HasThrow "perr" (ProductionError p i)) via
    MonadError (Except (ProductionError p i))
    
  --deriving (HasThrow "perr" (ProductionError p)) via
    --MonadError (Except (ProductionError p))

    