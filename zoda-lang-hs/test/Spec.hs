import ClassyPrelude
import Test.Hspec
import Test.Hspec.Runner
import qualified ParserSpec as Parsing

main :: IO ()
main = hspec $ do
  Parsing.test
