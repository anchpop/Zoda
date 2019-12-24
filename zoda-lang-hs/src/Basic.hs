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
data ProductionError m i = ZodaSyntaxError (ParseErrorBundle String ZodaParseError) 
                             | ValueRedeclaration (Declaration Parsed m i) 
                             | UndeclaredValuesReferenced [(i, SourcePosition)] 
                             | NoMain (Module Parsed m i) 
                             | MultipleValueUse [(i, ExpressionX Parsed m i)] 
                             | IncorrectNumArgumentsProvided (ExpressionX Parsed m i)
                             | TypeDecWithoutValueDef
                             | TypeErr
  
  --deriving (HasThrow "perr" (ProductionError p)) via
    --MonadError (Except (ProductionError p))

deriving instance (ConstraintX Show Parsed m i, ConstraintX Nominal Parsed m i, ConstraintX NominalShow Parsed m i, ConstraintX NominalSupport Parsed m i, Show m, Show i, Nominal m, Nominal i, NominalShow m, NominalShow i, NominalSupport m, NominalSupport i) => Show (ProductionError m i) 
deriving instance (ConstraintX Eq Parsed m i, ConstraintX Nominal Parsed m i, Eq m, Eq i, Nominal m, Nominal i) => Eq (ProductionError m i) 

instance ShowErrorComponent ZodaParseError where 
  showErrorComponent = show 