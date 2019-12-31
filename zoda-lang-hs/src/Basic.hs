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
data ProductionError i m = ZodaSyntaxError (ParseErrorBundle String ZodaParseError) 
                             | ValueRedeclaration [[(i, DeclarationInfo Parsed i  (i, SourcePosition) i)]] 
                             | UndeclaredValuesReferenced [(i, SourcePosition)] 
                             | NoMain (Module Parsed i m) 
                             | MultipleValueUse [(i, ExpressionX Parsed i m)] 
                             | IncorrectNumArgumentsProvided (ExpressionX Parsed i m)
                             | TypeDecWithoutValueDef
                             | TypeErr
  
  --deriving (HasThrow "perr" (ProductionError p)) via
    --MonadError (Except (ProductionError p))

deriving instance (ConstraintX Show Parsed i m, ConstraintX Nominal Parsed i m, ConstraintX NominalShow Parsed i m, ConstraintX NominalSupport Parsed i m, Show m, Show i, Nominal m, Nominal i, NominalShow m, NominalShow i, NominalSupport m, NominalSupport i) => Show (ProductionError i m) 
deriving instance (ConstraintX Eq Parsed i m, ConstraintX Nominal Parsed i m, Eq m, Eq i, Nominal m, Nominal i) => Eq (ProductionError i m) 

instance ShowErrorComponent ZodaParseError where 
  showErrorComponent = show 