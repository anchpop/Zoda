module Basic where
import ClassyPrelude
import Ast
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Capability.Error
import Capability.Writer
import qualified CopyPropagatedProgram as CPP
import Control.Monad.Except (ExceptT (..), Except)


data ProductionError t p i = ZodaSyntaxError (ParseErrorBundle String Void) 
                           | ValueRedeclaration (Declaration t p i) 
                           | UndeclaredValuesReferenced [LowercaseIdentifier t p i] 
                           | NoMain (Module t p i) 
                           | MultipleValueUse [(i, Expression t p i)] 
                           | IncorrectNumArgumentsProvided (Expression t p i)
  deriving (Show, Eq)
  deriving anyclass Exception

newtype M t p i r = M { runM :: Either (ProductionError t p i) r }
  deriving (Functor, Applicative, Monad) via Either (ProductionError t p i)
  deriving (HasThrow "perr" (ProductionError t p i)) via
    MonadError (Except (ProductionError t p i))
    
  --deriving (HasThrow "perr" (ProductionError p)) via
    --MonadError (Except (ProductionError p))

    