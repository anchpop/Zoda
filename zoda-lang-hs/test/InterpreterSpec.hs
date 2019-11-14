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
import Interpreter

test :: SpecWith ()
test = parallel $ do
  describe "Interpreter.produceProgram" $ do
    it "evaluates simplest possible program" $ do
      let example = "module i `test module`\n\
                    \main = 3\n\
                    \"
                    
      (produceProgram example) `shouldBe` pure 3

      
    it "evaluates simplest possible program where main is behind a layer of indirection" $ do
      let example = "module i `test module`\n\
                    \test = 3\n\
                    \main = test\n\
                    \"
                    
      (produceProgram example) `shouldBe` pure 3

    it "evaluates simple addition" $ do
      let example = "module i `test module`\n\
                    \test = 3 + 5\n\
                    \main = test\n\
                    \"
                    
      (produceProgram example) `shouldBe` pure 8
      
    it "evaluates identity function" $ do
      let example = "module i `test module`\n\
                    \test = 3 + 5\n\
                    \func = |a| a \n\
                    \main = test.func\n\
                    \"
                    
      (getRightZSE $ produceProgram example) `shouldBe` 8

      
    it "evaluates addition function" $ do
      let example = "module i `test module`\n\
                      \main = 3.(|a| (a + 4))\n\
                      \"
                      
      (produceProgram example) `shouldBe` pure 7

      
    it "evaluates addition function behind indirection " $ do
      let example = "module i `test module`\n\
                    \test = 3 + 1 \n\
                    \func = |a| (a + 4) \n\
                    \main = test.func\n\
                    \"
                    
      (produceProgram example) `shouldBe` pure 8

    it "evaluates higher order functions " $ do
      let example = "module i `test module`\n\
                    \test = 3 + 1 \n\
                    \func = |a| (a + 4) \n\
                    \ho = |a| (|b| a.b)\n\
                    \main = func.(test.ho)\n\
                    \"
                    
      (getRightZSE $ produceProgram example) `shouldBe` 8

    it "evaluates functions with multiple arguments " $ do
      let example = "module i `test module`\n\
                    \test = 3 + 1 \n\
                    \func = |a| (a + 4) \n\
                    \ho = |a, b| (a.b)\n\
                    \main = test.ho(func)\n\
                    \"
                    
      (getRightZSE $ produceProgram example) `shouldBe` 8