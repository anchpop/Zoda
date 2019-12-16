module TypecheckerSpec where
import ClassyPrelude
import Basic

import Data.Either

import Text.Megaparsec hiding (State)
import Data.Void
import Test.Hspec
import Test.Hspec.Runner
import qualified Data.Map.Justified as Map

import Data.Ratio

import Data.Foldable (for_)

import Parser
import Ast

import Interpreter
import Typechecker

typcheckS :: Text -> Either (ProductionError Untyped SourcePosition (Text, SourcePosition) Text) ()
typcheckS str = do 
  modu <- parseModule str
  result <- copyPropagated primatives modu typecheck
  result

test :: SpecWith ()
test = parallel $ do
  describe "Typechecker.typecheck" $ do
    it "typechecks numbers" $ do
      let exampleModule = "module i `test module`\n\
                    \test = 3 + 5\n\
                    \main = test\n\
                    \" :: Text
      typcheckS exampleModule `shouldBe` Right ()
      
    it "typechecks functions" $ do
      let exampleModule = "module i `test module`\n\
                    \test = 3 + 5\n\
                    \func = |a : Nat| a \n\
                    \main = test.func\n\
                    \" :: Text
      typcheckS exampleModule `shouldBe` Right ()
      
    it "typechecks generic functions" $ do
      let exampleModule = "module i `test module`\n\
                    \test = 3 + 5\n\
                    \func = |a : Type, b : a| b \n\
                    \main = Nat.func(3)\n\
                    \" :: Text
      typcheckS exampleModule `shouldBe` Right ()
      
    it "doesn't allow you to use a number where a type is expected" $ do
      let exampleModule = "module i `test module`\n\
                    \test = 3 + 5\n\
                    \func = |a : Type, b : a| b \n\
                    \main = 3.func(Nat)\n\
                    \" :: Text
      typcheckS exampleModule `shouldSatisfy` isLeft

      
      
    it "typechecks Nat annotations" $ do
      let exampleModule = "module i `test module`\n\
                    \test : Nat\n\
                    \test = 3 + 5\n\
                    \main = test\n\
                    \" :: Text
      getRightZSE (typcheckS exampleModule) `shouldBe` ()

    it "typechecks Nat annotations behind indirection" $ do
      let exampleModule = "module i `test module`\n\
                    \test = 3 + 5\n\
                    \main : Nat\n\
                    \main = test\n\
                    \" :: Text
      typcheckS exampleModule `shouldBe` Right ()

    it "typechecks function annotations" $ do
      let exampleModule = "module i `test module`\n\
                    \test = 3 + 5\n\
                    \func : (Nat) -> Nat\n\
                    \func = |a : Nat| a \n\
                    \main = test.func\n\
                    \" :: Text
      typcheckS exampleModule `shouldBe` Right ()

    it "doesn't allow wrong annotations" $ do
      let exampleModule = "module i `test module`\n\
                    \test = 3 + 5\n\
                    \func : Nat\n\
                    \func = |a : Nat| a \n\
                    \main = test.func\n\
                    \" :: Text
      typcheckS exampleModule `shouldSatisfy` isLeft

    it "doesn't allow you to add functions" $ do
      let exampleModule = "module i `test module`\n\
                    \test = 3 + 5\n\
                    \func = |a : Nat| a \n\
                    \main = 3 + func\n\
                    \" :: Text
      typcheckS exampleModule `shouldSatisfy` isLeft


      

    it "works with ADTs" $ do
      let exampleModule = "module i `test module`\n\
                    \type Bool : Type = \n\
                    \    True : Bool\n\
                    \  | False : Bool\n\
                    \test = 3 + 5\n\
                    \func = |a : Nat, b : Bool| a \n\
                    \main = test.func(True)\n\
                    \" :: Text
      typcheckS exampleModule `shouldSatisfy` isRight