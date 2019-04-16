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
                    
      (runM (parseModule example >>= produceProgram)) `shouldBe` Right (NumberLiteralExpression (NumberLiteral (3 % 1) (SourcePosition "module" 3 8 3 9)) (SourcePosition "module" 3 8 3 9))

      
    it "parses simplest possible program where main is behind a layer of indirection" $ do
      let example = "module i `test module`\n\
                    \test = 3\n\
                    \main = test\n\
                    \"
                    
      (runM (parseModule example >>= produceProgram)) `shouldBe` Right (NumberLiteralExpression (NumberLiteral (3 % 1) (SourcePosition "module" 2 8 2 9)) (SourcePosition "module" 2 8 2 9))