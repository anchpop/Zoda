module ParserSpec where
import ClassyPrelude

import Data.Either

import Text.Megaparsec hiding (State)
import Data.Void
import Test.Hspec
import Test.Hspec.Runner
import Data.Ratio
import Data.Foldable (for_)
import qualified Data.List.NonEmpty as NonEmpty

import Parser
import Ast

import Nominal hiding ((.))

--shouldParseTo :: (Show a, Eq a) => Either (ParseErrorBundle String Void) a -> a -> Expectation
shouldParseTo a b = getRight a `shouldBe` b
shouldParseToMD a b = ((parsedToNormalizedPlain $ getRight a) `shouldBe` b) 
--shouldNotParse :: (Show a, Show b) => Either a b -> Expectation
shouldNotParse a = a `shouldSatisfy` (isLeft)

test :: SpecWith ()
test = parallel $ do
  describe "Parser.tinydocP" $ do
    it "parses tinydocs" $ do
      parseSomething "`test test test`" (tinydocP) `shouldParseTo` Tinydoc "test test test"

      parseSomething "`Test-case! `"    (tinydocP) `shouldParseTo` Tinydoc "Test-case! " 

    it "doesn't accept what it shouldn't" $ do
      shouldNotParse $ parseSomething "`test" (tinydocP)
      shouldNotParse $ parseSomething "`test\n`" (tinydocP)




  describe "Parser.numberLiteralP" $ do
    it "parses number literals" $ do
      parseSomething "3"      numberLiteralP `shouldParseTo` 3 
      parseSomething "-3"     numberLiteralP `shouldParseTo` (negate 3)
      parseSomething "-3.5"   numberLiteralP `shouldParseTo` (negate 3.5)
      parseSomething "-13.5"  numberLiteralP `shouldParseTo` (negate 13.5)
      parseSomething "-13.50" numberLiteralP `shouldParseTo` (negate 13.5)

    it "doesn't accept what it shouldn't" $ do
      shouldNotParse $ parseSomething ".3" numberLiteralP
      shouldNotParse $ parseSomething "-.3" numberLiteralP
      shouldNotParse $ parseSomething "3." numberLiteralP
      shouldNotParse $ parseSomething "03" numberLiteralP




  describe "Parser.expressionP" $ do
    it "parses number literals" $ do
      parseSomething "3" expressionP `shouldParseTo` (NumberLiteralX (Sp $ SourcePosition "no_file" 1 1 1 2) 3) 

    it "parses addition" $ do
      parseSomething "3 + 1" expressionP `shouldParseTo` (AddExpressionX (Sp $ SourcePosition "no_file" 1 1 1 6) (NumberLiteralX (Sp $ SourcePosition "no_file" 1 1 1 3) (3 % 1)) (NumberLiteralX (Sp $ SourcePosition "no_file" 1 5 1 6) (1 % 1))) 

    it "parses addition in parentheses" $ do
      parseSomething "(3 + 1)" expressionP `shouldParseTo` ParenthesizedExpressionX (Sp $ SourcePosition "no_file" 1 1 1 8) (AddExpressionX (Sp $ SourcePosition "no_file" 1 2 1 7) (NumberLiteralX (Sp $ SourcePosition "no_file" 1 2 1 4) (3 % 1)) (NumberLiteralX (Sp $ SourcePosition "no_file" 1 6 1 7) (1 % 1)))

    it "parses identifiers" $ do
      parseSomething "test" expressionP `shouldParseTo` (ReferenceVariableX (Sp $ SourcePosition "no_file" 1 1 1 5) "test" ("test", SourcePosition "no_file" 1 1 1 5))


    it "parses single argument functions" $ do
      parseSomething "|a : Type| a" expressionP `shouldParseToMD` (with_fresh_named "a" $ \a -> FunctionLiteralExpression (LastArg (ReferenceVariable () ("Type",(SourcePosition "no_file" 1 6 1 10))) ((a,np) :. LambdaVariable (a,()))))


    it "parses multi argument functions" $ do
      parseSomething "|a : 1, b : a, c : b| 3" expressionP `shouldParseToMD` (with_fresh_named "a" $ \a -> with_fresh_named "b" $ \b -> with_fresh_named "c" $ \c -> (FunctionLiteralExpression (Arg (NumberLiteral (1 % 1)) ((a,np) :. Arg (LambdaVariable (a,())) ((b,np) :. LastArg (LambdaVariable (b,())) ((c,np) :. NumberLiteral (3 % 1)))))))

    it "parses function applications" $ do
      parseSomething "3.b" expressionP `shouldParseToMD` FunctionApplicationExpression (ReferenceVariable () ("b",(SourcePosition "no_file" 1 3 1 4))) (NumberLiteral (3 % 1) NonEmpty.:| [])

    it "parses telescopes " $ do
      parseSomething "(a : 3, b : a) -> b" expressionP `shouldParseToMD` with_fresh_named "a" (\a -> with_fresh_named "b" (\b -> (TArrowBinding (Scope (NumberLiteral (3 % 1)) (Just (a,np) :. Pi (LambdaVariable (a,())) (Just (b,np) :. LambdaVariable (b,())))))))
    
    it "parses function that takes a number and adds 4" $ do
      parseSomething "|a : Nat| (a + 4)" expressionP `shouldParseToMD` with_fresh_named "a" (\a -> FunctionLiteralExpression (LastArg (ReferenceVariable () ("Nat",(SourcePosition "no_file" 1 6 1 9))) ((a,NoBind ()) :. ParenthesizedExpression (AddExpression (LambdaVariable (a,())) (NumberLiteral (4 % 1))))))

    it "parses function application inline" $ do
      parseSomething "3.(|a : Nat| (a + 4))" expressionP `shouldParseToMD` with_fresh_named "a" (\a -> FunctionApplicationExpression (ParenthesizedExpression (FunctionLiteralExpression (LastArg (ReferenceVariable () ("Nat",(SourcePosition "no_file" 1 9 1 12))) ((a,NoBind ()) :. ParenthesizedExpression (AddExpression (LambdaVariable (a,())) (NumberLiteral (4 % 1))))))) (NumberLiteral (3 % 1) NonEmpty.:| []))

    it "doesn't leak symbols from arrows" $ do
      parseSomething "((a : Type) -> Nat) + a" expressionP `shouldParseToMD` (AddExpression (ParenthesizedExpression (with_fresh_named "a" (\a -> TArrowBinding (Pi (ReferenceVariable () ("Type",(SourcePosition "no_file" 1 7 1 11))) (Just (a,NoBind ()) :. ReferenceVariable () ("Nat",SourcePosition "no_file" 1 16 1 19)))))) (ReferenceVariable () ("a",SourcePosition "no_file" 1 23 1 24)))

    it "doesn't leak symbols from functions" $ do
      parseSomething "(|a : Nat| a) + a" expressionP `shouldParseToMD` (AddExpression (ParenthesizedExpression (with_fresh_named "a" (\a -> FunctionLiteralExpression (LastArg (ReferenceVariable () ("Nat",(SourcePosition "no_file" 1 7 1 10))) ((a,NoBind ()) :. LambdaVariable (a,())))))) (ReferenceVariable () ("a",(SourcePosition "no_file" 1 17 1 18))))

  describe "Parser.valueDefinitionP" $ do
    it "allows assignment to number literals" $ do
      parseSomething "i = 3" valueDefinitionP `shouldParseTo` ValueDefinitionX (Sp $ SourcePosition "no_file" 1 1 1 6)
        "i" (NumberLiteralX (Sp $ SourcePosition "no_file" 1 5 1 6) 3)
        

      
  describe "Parser.valueDefinitionP" $ do
    it "parses indented newlines" $ do
      parseSomething "i = \n\
                     \  3 + 4" valueDefinitionP `shouldParseTo` ValueDefinitionX (Sp $ SourcePosition "no_file" 1 1 2 8) "i" (AddExpressionX (Sp $ SourcePosition "no_file" 2 3 2 8) (NumberLiteralX (Sp $ SourcePosition "no_file" 2 3 2 5) (3 % 1)) (NumberLiteralX (Sp $ SourcePosition "no_file" 2 7 2 8) (4 % 1))) 
      parseSomething "i = \n\
                     \3 + 4" valueDefinitionP `shouldSatisfy` isLeft

  describe "Parser.valueDefinitionWithAnnotationP" $ do
    it "allows annotation and assignment to number literals" $ do
      parseSomething "i : 3 + 4\n\
                     \i = 3" valueDefinitionWithAnnotationP `shouldParseTo` ValueDefinitionAnnotatedX (SpPair ((SourcePosition "no_file" 2 1 2 6),(SourcePosition "no_file" 1 1 1 10))) "i" (NumberLiteralX (Sp {sp = (SourcePosition "no_file" 2 5 2 6)}) (3 % 1)) (AddExpressionX (Sp {sp = (SourcePosition "no_file" 1 5 1 10)}) (NumberLiteralX (Sp {sp = (SourcePosition "no_file" 1 5 1 7)}) (3 % 1)) (NumberLiteralX (Sp {sp = (SourcePosition "no_file" 1 9 1 10)}) (4 % 1)))
    it "allows annotation and assignment to number literals with indentation" $ do
      parseSomething "i : \n\
                     \  3 + 4\n\
                     \i = 3" valueDefinitionWithAnnotationP `shouldParseTo` ValueDefinitionAnnotatedX (SpPair ((SourcePosition "no_file" 3 1 3 6),(SourcePosition "no_file" 1 1 2 8))) "i" (NumberLiteralX (Sp {sp = (SourcePosition "no_file" 3 5 3 6)}) (3 % 1)) (AddExpressionX (Sp {sp = (SourcePosition "no_file" 2 3 2 8)}) (NumberLiteralX (Sp {sp = (SourcePosition "no_file" 2 3 2 5)}) (3 % 1)) (NumberLiteralX (Sp {sp = (SourcePosition "no_file" 2 7 2 8)}) (4 % 1)))
      parseSomething "i : \n\
                     \  3 + 4" valueDefinitionWithAnnotationP `shouldSatisfy` isLeft
    
  describe "Parser.typeDefinitionP" $ do
    it "allows the creation of a type to represent booleans" $ do
      parseSomething "type Bool : Type = \n\
                     \    True : Bool\n\
                     \  | False : Bool" typeDefinitionP `shouldParseTo` TypeDefinitionX (Sp {sp = (SourcePosition "no_file" 1 1 3 17)}) "Bool" (ReferenceVariableX (Sp {sp = (SourcePosition "no_file" 1 13 1 18)}) "Type" ("Type",(SourcePosition "no_file" 1 13 1 18))) [(Sp {sp = (SourcePosition "no_file" 2 5 2 16)},"True",ReferenceVariableX (Sp {sp = (SourcePosition "no_file" 2 12 2 16)}) "Bool" ("Bool",(SourcePosition "no_file" 2 12 2 16))),(Sp {sp = (SourcePosition "no_file" 3 5 3 17)},"False",ReferenceVariableX (Sp {sp = (SourcePosition "no_file" 3 13 3 17)}) "Bool" ("Bool",(SourcePosition "no_file" 3 13 3 17)))]


  describe "Parser.parseModule" $ do
    it "parses modules with declaration" $ do 
      let exampleModule = "module i `test module`\n\
                    \main = 3\n\
                    \" :: Text
      parseModule exampleModule `shouldSatisfy` isRight

    it "parses modules with multiple declarations" $ do 
      let exampleModule = "module i `test module`\n\
                    \test = 3\n\
                    \main = test\n\
                    \" :: Text
      parseModule exampleModule `shouldSatisfy` isRight

    it "parses modules with multiple declarations and annotations" $ do 
      let exampleModule = "module i `test module`\n\
                    \test : Nat\n\
                    \test = 3\n\
                    \main = test\n\
                    \" :: Text
      let exampleModule2 = "module i `test module`\n\
                    \test : Nat\n\
                    \test = 3\n\
                    \main : Nat\n\
                    \main = test\n\
                    \" :: Text
      parseModule exampleModule  `shouldSatisfy` isRight
      parseModule exampleModule2 `shouldSatisfy` isRight
      
    it "parses modules with multiple declarations and type definitions " $ do 
      let exampleModule = "module i `test module`\n\
                    \test = 3\n\
                    \type Bool : Type = \n\
                    \    True : Bool\n\
                    \  | False : Bool\n\
                    \type Dool : Type = \n\
                    \    True : Dool\n\
                    \  | False : Dool\n\
                    \main = test\n" :: Text
      parseModule exampleModule `shouldBe` Right (
            Module (ModuleHeader "i" (Tinydoc "test module")) [ValueDefinitionX (Sp {sp = (SourcePosition "module" 2 1 2 9)}) "test" (NumberLiteralX (Sp {sp = (SourcePosition "module" 2 8 2 9)}) (3 % 1)),TypeDefinitionX (Sp {sp = (SourcePosition "module" 3 1 5 17)}) "Bool" (ReferenceVariableX (Sp {sp = (SourcePosition "module" 3 13 3 18)}) "Type" ("Type",(SourcePosition "module" 3 13 3 18))) [(Sp {sp = (SourcePosition "module" 4 5 4 16)},"True",ReferenceVariableX (Sp {sp = (SourcePosition "module" 4 12 4 16)}) "Bool" ("Bool",(SourcePosition "module" 4 12 4 16))),(Sp {sp = (SourcePosition "module" 5 5 5 17)},"False",ReferenceVariableX (Sp {sp = (SourcePosition "module" 5 13 5 17)}) "Bool" ("Bool",(SourcePosition "module" 5 13 5 17)))],TypeDefinitionX (Sp {sp = (SourcePosition "module" 6 1 8 17)}) "Dool" (ReferenceVariableX (Sp {sp = (SourcePosition "module" 6 13 6 18)}) "Type" ("Type",(SourcePosition "module" 6 13 6 18))) [(Sp {sp = (SourcePosition "module" 7 5 7 16)},"True",ReferenceVariableX (Sp {sp = (SourcePosition "module" 7 12 7 16)}) "Dool" ("Dool",(SourcePosition "module" 7 12 7 16))),(Sp {sp = (SourcePosition "module" 8 5 8 17)},"False",ReferenceVariableX (Sp {sp = (SourcePosition "module" 8 13 8 17)}) "Dool" ("Dool",(SourcePosition "module" 8 13 8 17)))],ValueDefinitionX (Sp {sp = (SourcePosition "module" 9 1 9 12)}) "main" (ReferenceVariableX (Sp {sp = (SourcePosition "module" 9 8 9 12)}) "test" ("test",(SourcePosition "module" 9 8 9 12)))
          ]
        )
        

np = NoBind ()