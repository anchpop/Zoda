module ParserSpec where
import ClassyPrelude

import Data.Either

import Text.Megaparsec hiding (State)
import Data.Void
import Test.Hspec
import Test.Hspec.Runner
import Data.Ratio
import Data.Foldable (for_)

import Parser
import Ast

shouldParseTo :: (Show a, Eq a) => Either (ParseErrorBundle String Void) a -> a -> Expectation
shouldParseTo a b = getRight a `shouldBe` b
shouldNotParse :: (Show a, Show b) => Either a b -> Expectation
shouldNotParse a = a `shouldSatisfy` (isLeft)

test :: SpecWith ()
test = parallel $ do
  describe "Parser.lowercaseIdentifierP" $ do
    it "parses lowercase identifiers" $ do
      parseSomething "module"    lowercaseIdentifierP `shouldParseTo` LowercaseIdentifier "module" (SourcePosition "no_file" 1 1 1 7)

      parseSomething "test-case" lowercaseIdentifierP `shouldParseTo` LowercaseIdentifier "test-case" (SourcePosition "no_file" 1 1 1 10)
    it "doesn't accept what it shouldn't" $ do
      shouldNotParse $ parseSomething "test case" lowercaseIdentifierP
      shouldNotParse $ parseSomething "Test" lowercaseIdentifierP
      shouldNotParse $ parseSomething "-test" lowercaseIdentifierP



  describe "Parser.uppercaseIdentifierP" $ do
    it "parses Uppercase identifiers" $ do
      parseSomething "Module"    uppercaseIdentifierP `shouldParseTo` UppercaseIdentifier "Module" (SourcePosition "no_file" 1 1 1 7)

      parseSomething "Test-case" uppercaseIdentifierP `shouldParseTo` UppercaseIdentifier "Test-case" (SourcePosition "no_file" 1 1 1 10)

    it "doesn't accept what it shouldn't" $ do
      shouldNotParse $ parseSomething "Test case" uppercaseIdentifierP 
      shouldNotParse $ parseSomething "test" uppercaseIdentifierP
      shouldNotParse $ parseSomething "-Test" uppercaseIdentifierP



  describe "Parser.tinydocP" $ do
    it "parses tinydocs" $ do
      parseSomething "`test test test`" tinydocP `shouldParseTo` Tinydoc "test test test" (SourcePosition "no_file" 1 1 1 17)

      parseSomething "`Test-case! `"    tinydocP `shouldParseTo` Tinydoc "Test-case! " (SourcePosition "no_file" 1 1 1 14)

    it "doesn't accept what it shouldn't" $ do
      shouldNotParse $ parseSomething "`test" tinydocP
      shouldNotParse $ parseSomething "`test\n`" tinydocP




  describe "Parser.numberLiteralP" $ do
    it "parses number literals" $ do
      parseSomething "3"      numberLiteralP `shouldParseTo` NumberLiteral 3 (SourcePosition "no_file" 1 1 1 2)
      parseSomething "-3"     numberLiteralP `shouldParseTo` NumberLiteral (negate 3) (SourcePosition "no_file" 1 1 1 3)
      parseSomething "-3.5"   numberLiteralP `shouldParseTo` NumberLiteral (negate 3.5) (SourcePosition "no_file" 1 1 1 5)
      parseSomething "-13.5"  numberLiteralP `shouldParseTo` NumberLiteral (negate 13.5) (SourcePosition "no_file" 1 1 1 6)
      parseSomething "-13.50" numberLiteralP `shouldParseTo` NumberLiteral (negate 13.5) (SourcePosition "no_file" 1 1 1 7)

    it "doesn't accept what it shouldn't" $ do
      shouldNotParse $ parseSomething ".3" numberLiteralP
      shouldNotParse $ parseSomething "-.3" numberLiteralP
      shouldNotParse $ parseSomething "3." numberLiteralP
      shouldNotParse $ parseSomething "03" numberLiteralP




  describe "Parser.expressionP" $ do
    it "parses number literals" $ do
      parseSomething "3" expressionP `shouldParseTo` NumberLiteralExpression (NumberLiteral 3 (SourcePosition "no_file" 1 1 1 2)) (SourcePosition "no_file" 1 1 1 2)

    it "parses identifiers" $ do
      parseSomething "test" expressionP `shouldParseTo` IdentifierExpression (LowercaseIdentifier "test" (SourcePosition "no_file" 1 1 1 5)) (SourcePosition "no_file" 1 1 1 5)


    it "parses functions" $ do
      parseSomething "|a, b, c| -> 3" expressionP `shouldParseTo` FunctionLiteralExpression
        ( FunctionLiteral
          [ LowercaseIdentifier "a" (SourcePosition "no_file" 1 2 1 3)
          , LowercaseIdentifier "b" (SourcePosition "no_file" 1 5 1 6)
          , LowercaseIdentifier "c" (SourcePosition "no_file" 1 8 1 9)
          ]
          (NumberLiteralExpression (NumberLiteral (3 % 1) (SourcePosition "no_file" 1 14 1 15)) (SourcePosition "no_file" 1 14 1 15))
          (SourcePosition "no_file" 1 1 1 15)
        )
        (SourcePosition "no_file" 1 1 1 15)



    it "parses function applications" $ do
      parseSomething "3.b" expressionP `shouldParseTo` FunctionApplicationExpression
        (NumberLiteralExpression (NumberLiteral (3 % 1) (SourcePosition "no_file" 1 1 1 2)) (SourcePosition "no_file" 1 1 1 2))
        (IdentifierExpression (LowercaseIdentifier "b" (SourcePosition "no_file" 1 3 1 4)) (SourcePosition "no_file" 1 3 1 4))
        (SourcePosition "no_file" 1 1 1 4)


  describe "Parser.declarationP" $ do
    it "allows assignment to number literals" $ do
      parseSomething "i = 3" declarationP `shouldParseTo` Declaration
        (LowercaseIdentifier "i" (SourcePosition "no_file" 1 1 1 2))
        (NumberLiteralExpression (NumberLiteral 3 (SourcePosition "no_file" 1 5 1 6)) (SourcePosition "no_file" 1 5 1 6))
        (SourcePosition "no_file" 1 1 1 6)
