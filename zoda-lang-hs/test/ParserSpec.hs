module ParserSpec where

import Data.Either

import Text.Megaparsec hiding (State)
import Data.Void
import Test.Hspec
import Test.Hspec.Runner

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
      parseSomething "module" lowercaseIdentifierP
        `shouldParseTo` LowercaseIdentifier "module" (SourcePosition "no_file" 1 1 1 7)

      parseSomething "test-case" lowercaseIdentifierP
        `shouldParseTo` LowercaseIdentifier "test-case" (SourcePosition "no_file" 1 1 1 10)

      parseSomething "test case" lowercaseIdentifierP
        `shouldParseTo` LowercaseIdentifier "test" (SourcePosition "no_file" 1 1 1 5)
    it "doesn't accept what it shouldn't" $ do
      shouldNotParse $ parseSomething "Test" lowercaseIdentifierP
      shouldNotParse $ parseSomething "-test" lowercaseIdentifierP



  describe "Parser.uppercaseIdentifierP" $ do
    it "parses Uppercase identifiers" $ do
      parseSomething "Module" uppercaseIdentifierP
        `shouldParseTo` UppercaseIdentifier "Module" (SourcePosition "no_file" 1 1 1 7)

      parseSomething "Test-case" uppercaseIdentifierP
        `shouldParseTo` UppercaseIdentifier "Test-case" (SourcePosition "no_file" 1 1 1 10)

      parseSomething "Test case" uppercaseIdentifierP
        `shouldParseTo` UppercaseIdentifier "Test" (SourcePosition "no_file" 1 1 1 5)
    it "doesn't accept what it shouldn't" $ do
      shouldNotParse $ parseSomething "test" uppercaseIdentifierP
      shouldNotParse $ parseSomething "-Test" uppercaseIdentifierP



  describe "Parser.tinydocP" $ do
    it "parses tinydocs" $ do
      parseSomething "`test test test`" tinydocP
        `shouldParseTo` Tinydoc "test test test" (SourcePosition "no_file" 1 1 1 17)

      parseSomething "`Test-case! `" tinydocP `shouldParseTo` Tinydoc "Test-case! " (SourcePosition "no_file" 1 1 1 14)

    it "doesn't accept what it shouldn't" $ do
      shouldNotParse $ parseSomething "`test" tinydocP
      shouldNotParse $ parseSomething "`test\n`" tinydocP




  describe "Parser.numberLiteralP" $ do
    it "parses number literals" $ do
      parseSomething "3"     numberLiteralP `shouldParseTo` NumberLiteral 3 (SourcePosition "no_file" 1 1 1 2)
      parseSomething "-3"    numberLiteralP `shouldParseTo` NumberLiteral (negate 3) (SourcePosition "no_file" 1 1 1 3)
      parseSomething "-3.5" numberLiteralP `shouldParseTo` NumberLiteral (negate 3.5) (SourcePosition "no_file" 1 1 1 5)
      parseSomething "-13.5" numberLiteralP
        `shouldParseTo` NumberLiteral (negate 13.5) (SourcePosition "no_file" 1 1 1 6)
      parseSomething "-13.50" numberLiteralP
        `shouldParseTo` NumberLiteral (negate 13.5) (SourcePosition "no_file" 1 1 1 7)

    it "doesn't accept what it shouldn't" $ do
      shouldNotParse $ parseSomething ".3" numberLiteralP
      shouldNotParse $ parseSomething "-.3" numberLiteralP
      shouldNotParse $ parseSomething "3." numberLiteralP
      shouldNotParse $ parseSomething "03" numberLiteralP


