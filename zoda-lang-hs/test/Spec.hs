import ClassyPrelude
import Test.Hspec
import Test.Hspec.Runner
import qualified ParserSpec as Parsing
import qualified InterpreterSpec as Interpreting
import qualified TypecheckerSpec as Typechecker

main :: IO ()
main = hspec $ do
  Parsing.test
  Interpreting.test
  Typechecker.test
