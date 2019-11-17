module ParserSpec where
import ClassyPrelude

import Data.Either

import Text.Megaparsec hiding (State)
import Data.Void
import Test.Hspec
import Test.Hspec.Runner
import Data.Ratio
import Data.Foldable (for_)
import qualified Data.List.NonEmpty as NonEmpty

import Parser
import Ast

import Nominal hiding ((.))

--shouldParseTo :: (Show a, Eq a) => Either (ParseErrorBundle String Void) a -> a -> Expectation
shouldParseTo a b = getRight a `shouldBe` b
--shouldNotParse :: (Show a, Show b) => Either a b -> Expectation
shouldNotParse a = a `shouldSatisfy` (isLeft)

test :: SpecWith ()
test = parallel $ do
  describe "Parser.tinydocP" $ do
    it "parses tinydocs" $ do
      parseSomething "`test test test`" (sourcePosWrapper tinydocP) `shouldParseTo` Tinydoc "test test test" (SourcePosition "no_file" 1 1 1 17)

      parseSomething "`Test-case! `"    (sourcePosWrapper tinydocP) `shouldParseTo` Tinydoc "Test-case! " (SourcePosition "no_file" 1 1 1 14)

    it "doesn't accept what it shouldn't" $ do
      shouldNotParse $ parseSomething "`test" (sourcePosWrapper tinydocP)
      shouldNotParse $ parseSomething "`test\n`" (sourcePosWrapper tinydocP)




  describe "Parser.numberLiteralP" $ do
    it "parses number literals" $ do
      parseSomething "3"      numberLiteralP `shouldParseTo` 3 
      parseSomething "-3"     numberLiteralP `shouldParseTo` (negate 3)
      parseSomething "-3.5"   numberLiteralP `shouldParseTo` (negate 3.5)
      parseSomething "-13.5"  numberLiteralP `shouldParseTo` (negate 13.5)
      parseSomething "-13.50" numberLiteralP `shouldParseTo` (negate 13.5)

    it "doesn't accept what it shouldn't" $ do
      shouldNotParse $ parseSomething ".3" numberLiteralP
      shouldNotParse $ parseSomething "-.3" numberLiteralP
      shouldNotParse $ parseSomething "3." numberLiteralP
      shouldNotParse $ parseSomething "03" numberLiteralP




  describe "Parser.expressionP" $ do
    it "parses number literals" $ do
      parseSomething "3" expressionP `shouldParseTo` (NumberLiteral 3 Untyped (SourcePosition "no_file" 1 1 1 2)) 

    it "parses identifiers" $ do
      parseSomething "test" expressionP `shouldParseTo` (ReferenceVariable "test" ("test", SourcePosition "no_file" 1 1 1 5) Untyped (SourcePosition "no_file" 1 1 1 5))


    it "parses single argument functions" $ do
      parseSomething "|a : 0| a" expressionP `shouldParseTo` (with_fresh_named "a" $ \a -> FunctionLiteralExpression (LastArg (((NoBind "a",(a,NoBind (SourcePosition {_filePath = "no_file", _sourceLineStart = 1, _sourceColumnStart = 2, _sourceLineEnd = 1, _sourceColumnEnd = 3}))),NoBind (NumberLiteral (0 % 1) Untyped (SourcePosition {_filePath = "no_file", _sourceLineStart = 1, _sourceColumnStart = 6, _sourceLineEnd = 1, _sourceColumnEnd = 7}))) :. LambdaVariable ("a",a) Untyped (SourcePosition {_filePath = "no_file", _sourceLineStart = 1, _sourceColumnStart = 9, _sourceLineEnd = 1, _sourceColumnEnd = 10}))) Untyped (SourcePosition "no_file" 1 1 1 10))

    it "parses multi argument functions" $ do
      parseSomething "|a : 1, b : a, c : b| 3" expressionP `shouldParseTo` (with_fresh_named "a" $ \a -> with_fresh_named "b" $ \b -> with_fresh_named "c" $ \c -> (FunctionLiteralExpression (Arg (((NoBind "a",(a,NoBind (SourcePosition "no_file" 1 2 1 3))),NoBind (NumberLiteral (1 % 1) Untyped (SourcePosition "no_file" 1 6 1 7))) :. Arg (((NoBind "b",(b,NoBind (SourcePosition "no_file" 1 9 1 10))),NoBind (LambdaVariable ("a",a) Untyped (SourcePosition "no_file" 1 13 1 14))) :. LastArg (((NoBind "c",(c,NoBind (SourcePosition "no_file" 1 16 1 17))),NoBind (LambdaVariable ("b",b) Untyped (SourcePosition "no_file" 1 20 1 21))) :. NumberLiteral (3 % 1) Untyped (SourcePosition "no_file" 1 23 1 24))))) Untyped (SourcePosition "no_file" 1 1 1 24)))

    it "parses function applications" $ do
      parseSomething "3.b" expressionP `shouldParseTo` FunctionApplicationExpression (ReferenceVariable "b" ("b", SourcePosition "no_file" 1 3 1 4) Untyped (SourcePosition "no_file" 1 3 1 4))
        (NumberLiteral 3 Untyped (SourcePosition "no_file" 1 1 1 2) NonEmpty.:| [])
        Untyped (SourcePosition "no_file" 1 1 1 4)

    it "parses telescopes applications" $ do
      parseSomething "(a : 3, b : a) -> b" expressionP `shouldParseTo` with_fresh_named "a" (\a -> with_fresh_named "b" (\b -> (TArrowBinding (Scope ((Just (NoBind "a",(a,NoBind (SourcePosition "no_file" 1 2 1 3))),NoBind (NumberLiteral (3 % 1) Untyped (SourcePosition "no_file" 1 6 1 7))) :. Pi ((Just (NoBind "b",(b,NoBind (SourcePosition "no_file" 1 9 1 10))),NoBind (LambdaVariable ("a",a) Untyped (SourcePosition "no_file" 1 13 1 14))) :. LambdaVariable ("b",b) Untyped (SourcePosition "no_file" 1 19 1 20)))) Untyped  (SourcePosition "no_file" 1 1 1 20))))

  describe "Parser.declarationP" $ do
    it "allows assignment to number literals" $ do
      parseSomething "i = 3" declarationP `shouldParseTo` Declaration
        "i" (NumberLiteral 3 Untyped (SourcePosition "no_file" 1 5 1 6))
        (SourcePosition "no_file" 1 1 1 6)

