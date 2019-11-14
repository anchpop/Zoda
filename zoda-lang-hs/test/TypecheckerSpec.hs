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

import Typechecker

test :: SpecWith ()
test = parallel $ do
  describe "CopyPropagatedProgramConverter.evaluateMain" $ do
    it "typechecks on correct typing" $ do
      3 `shouldBe` 3