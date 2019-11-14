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
      parseSomething "test" expressionP `shouldParseTo` (ReferenceVariable "test" () Untyped (SourcePosition "no_file" 1 1 1 5))


    it "parses functions" $ do
      parseSomething "|a, b, c| 3" expressionP `shouldParseTo` FunctionLiteralExpression
          (with_fresh_named "a" $ \a -> with_fresh_named "b" $ \b -> with_fresh_named "c" $ \c ->   
          (((NoBind "a", (a, NoBind $ SourcePosition "no_file" 1 2 1 3)) NonEmpty.:|
          [ (NoBind "b", (b, NoBind $  SourcePosition "no_file" 1 5 1 6))
          , (NoBind "c", (c, NoBind $ SourcePosition "no_file" 1 8 1 9))
          ]) :. (NumberLiteral 3 Untyped (SourcePosition "no_file" 1 11 1 12))))
          Untyped (SourcePosition "no_file" 1 1 1 12)



    it "parses function applications" $ do
      parseSomething "3.b" expressionP `shouldParseTo` FunctionApplicationExpression (ReferenceVariable "b" () Untyped (SourcePosition "no_file" 1 3 1 4))
        (NumberLiteral 3 Untyped (SourcePosition "no_file" 1 1 1 2) NonEmpty.:| [])
        Untyped (SourcePosition "no_file" 1 1 1 4)

  describe "Parser.declarationP" $ do
    it "allows assignment to number literals" $ do
      parseSomething "i = 3" declarationP `shouldParseTo` Declaration
        "i" (NumberLiteral 3 Untyped (SourcePosition "no_file" 1 5 1 6))
        (SourcePosition "no_file" 1 1 1 6)
