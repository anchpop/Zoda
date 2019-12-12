module Basic where
import ClassyPrelude
import Ast
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified CopyPropagatedProgram as CPP
import Control.Monad.Except (ExceptT (..), Except)
import Nominal hiding ((.))


data ZodaParseError = DuplicateFunctionArgumentNames deriving (Show, Read, Ord, Eq, NominalSupport, NominalShow, Generic, Nominal)
data ProductionError t p m i = ZodaSyntaxError (ParseErrorBundle String ZodaParseError) 
                             | ValueRedeclaration (Declaration t p m i) 
                             | UndeclaredValuesReferenced [(i, p)] 
                             | NoMain (Module t p m i) 
                             | MultipleValueUse [(i, Expression t p m i)] 
                             | IncorrectNumArgumentsProvided (Expression t p m i)
                             | TypeDecWithoutValueDef
                             | TypeErr
  deriving (Show, Eq)
  deriving anyclass Exception
  
  --deriving (HasThrow "perr" (ProductionError p)) via
    --MonadError (Except (ProductionError p))

instance ShowErrorComponent ZodaParseError where 
  showErrorComponent = show 