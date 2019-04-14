import Test.Hspec
import Test.Hspec.Runner
import Data.Foldable (for_)
import Data.Void
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)


getRight :: Either (ParseErrorBundle String Void) b -> b
getRight (Right b  ) = b
getRight (Left  err) = error $ errorBundlePretty err


main :: IO ()
main = putStrLn "Test suite not yet implemented"
