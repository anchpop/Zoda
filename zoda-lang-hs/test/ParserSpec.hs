module ParserSpec where

import Test.Hspec
import Test.Hspec.Runner

import Data.Foldable (for_)

import Parser

test = do
  describe "Parser.Module" $ do
    it "parses the simplest possible module" $ do
      parseSomething "module test" moduleP `shouldBe` undefined