module InterpreterSpec where
import ClassyPrelude
import Basic

import Data.Either

import Text.Megaparsec hiding (State)
import Data.Void
import Test.Hspec
import Test.Hspec.Runner

import Data.Ratio
import Data.Either
import Data.Foldable (for_)

import Parser
import Ast
import Interpreter

test :: SpecWith ()
test = parallel $ do
  describe "Interpreter.produceProgram" $ do
    it "evaluates simplest possible program" $ do
      let exampleModule = "module i `test module`\n\
                    \main = 3\n\
                    \"
                    
      (getRightZSE $ produceProgram exampleModule) `shouldBe` 3

      
    it "evaluates simplest possible program where main is behind a layer of indirection" $ do
      let exampleModule = "module i `test module`\n\
                    \test = 3\n\
                    \main = test\n\
                    \"
                    
      (produceProgram exampleModule) `shouldBe` pure 3

    it "evaluates simple addition" $ do
      let exampleModule = "module i `test module`\n\
                    \test = 3 + 5\n\
                    \main = test\n\
                    \"
                    
      (produceProgram exampleModule) `shouldBe` pure 8
      
    it "evaluates identity function" $ do
      let exampleModule = "module i `test module`\n\
                    \test = 3 + 5\n\
                    \func = |a : Nat| a \n\
                    \main = test.func\n\
                    \"
                    
      (getRightZSE $ produceProgram exampleModule) `shouldBe` 8

      
    it "evaluates addition function" $ do
      let exampleModule = "module i `test module`\n\
                      \main = 3.(|a : Nat| (a + 4))\n\
                      \"
                      
      (getRightZSE $ produceProgram exampleModule) `shouldBe` 7

      
    it "evaluates addition function behind indirection " $ do
      let exampleModule = "module i `test module`\n\
                    \test = 3 + 1 \n\
                    \func = |a : Nat| (a + 4) \n\
                    \main = test.func\n\
                    \"
                    
      (getRightZSE $ produceProgram exampleModule) `shouldBe` 8

    it "evaluates higher order functions " $ do
      let exampleModule = "module i `test module`\n\
                    \test = 3 + 1 \n\
                    \func = |a : Nat| (a + 4) \n\
                    \ho = |a : Nat| (|b : (Nat) -> Nat| a.b)\n\
                    \main = func.(test.ho)\n\
                    \"
                    
      (getRightZSE $ produceProgram exampleModule) `shouldBe` 8

    it "evaluates functions with multiple arguments " $ do
      let exampleModule = "module i `test module`\n\
                    \test = 3 + 1 \n\
                    \func = |a : Nat| (a + 4) \n\
                    \ho = |a : Nat, b : (Nat) -> Nat| (a.b)\n\
                    \main = test.ho(func)\n\
                    \"
                    
      (getRightZSE $ produceProgram exampleModule) `shouldBe` 8