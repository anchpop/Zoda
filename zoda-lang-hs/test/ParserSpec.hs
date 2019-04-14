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



  describe "Parser.lowercaseIdentifierP" $ do
    it "parses lowercase identifiers" $ do
      parseSomething "module" lowercaseIdentifierP
        `shouldParseTo` LowercaseIdentifier "module" (SourcePosition "no_file" 1 1 1 7)