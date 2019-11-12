module InterpreterSpec where
import ClassyPrelude
import Basic

import Data.Either

import Text.Megaparsec hiding (State)
import Data.Void
import Test.Hspec
import Test.Hspec.Runner

import Data.Ratio

import Data.Foldable (for_)

import Parser
import Ast
import CopyPropagatedProgramConverter

test :: SpecWith ()
test = parallel $ do
  describe "CopyPropagatedProgramConverter.evaluateMain" $ do
    it "parses simplest possible program" $ do
      let example = "module i `test module`\n\
                    \test = 3\n\
                    \main = 3\n\
                    \"
                    
      (produceProgram example) `shouldBe` pure 3

      
    it "parses simplest possible program where main is behind a layer of indirection" $ do
      let example = "module i `test module`\n\
                    \test = 3\n\
                    \main = test\n\
                    \"
                    
      (produceProgram example) `shouldBe` pure 3