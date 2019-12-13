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
shouldParseToMD a b = ((normalizeExprMetadata $ getRight a) `shouldBe` b) 
--shouldNotParse :: (Show a, Show b) => Either a b -> Expectation
shouldNotParse a = a `shouldSatisfy` (isLeft)

test :: SpecWith ()
test = parallel $ do
  describe "Parser.tinydocP" $ do
    it "parses tinydocs" $ do
      parseSomething "`test test test`" (sourcePosWrapper tinydocP) `shouldParseTo` Tinydoc "test test test" (SourcePosition "no_file" 1 1 1 17)

      parseSomething "`Test-case! `"    (sourcePosWrapper tinydocP) `shouldParseTo` Tinydoc "Test-case! " (SourcePosition "no_file" 1 1 1 14)

    it "doesn't accept what it shouldn't" $ do
      shouldNotParse $ parseSomething "`test" (sourcePosWrapper tinydocP)
      shouldNotParse $ parseSomething "`test\n`" (sourcePosWrapper tinydocP)




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
      parseSomething "3" expressionP `shouldParseTo` (NumberLiteral 3 Untyped (SourcePosition "no_file" 1 1 1 2)) 

    it "parses addition" $ do
      parseSomething "3 + 1" expressionP `shouldParseTo` (AddExpression (NumberLiteral (3 % 1) Untyped (SourcePosition "no_file" 1 1 1 3)) (NumberLiteral (1 % 1) Untyped (SourcePosition "no_file" 1 5 1 6)) Untyped (SourcePosition "no_file" 1 1 1 6)) 

    it "parses addition in parentheses" $ do
      parseSomething "(3 + 1)" expressionP `shouldParseTo` ParenthesizedExpression (AddExpression (NumberLiteral (3 % 1) Untyped (SourcePosition "no_file" 1 2 1 4)) (NumberLiteral (1 % 1) Untyped (SourcePosition "no_file" 1 6 1 7)) Untyped (SourcePosition "no_file" 1 2 1 7)) Untyped (SourcePosition "no_file" 1 1 1 8)

    it "parses identifiers" $ do
      parseSomething "test" expressionP `shouldParseTo` (ReferenceVariable "test" ("test", SourcePosition "no_file" 1 1 1 5) Untyped (SourcePosition "no_file" 1 1 1 5))


    it "parses single argument functions" $ do
      parseSomething "|a : Type| a" expressionP `shouldParseToMD` (with_fresh_named "a" $ \a -> FunctionLiteralExpression (LastArg (ReferenceVariable () ("Type",(SourcePosition "no_file" 1 6 1 10)) () ()) ((a,np,np) :. LambdaVariable (a,()) () ())) () ())


    it "parses multi argument functions" $ do
      parseSomething "|a : 1, b : a, c : b| 3" expressionP `shouldParseToMD` (with_fresh_named "a" $ \a -> with_fresh_named "b" $ \b -> with_fresh_named "c" $ \c -> (FunctionLiteralExpression (Arg (NumberLiteral (1 % 1) () ()) ((a,np,np) :. Arg (LambdaVariable (a,()) () ()) ((b,np,np) :. LastArg (LambdaVariable (b,()) () ()) ((c,np,np) :. NumberLiteral (3 % 1) () ())))) () ()))

    it "parses function applications" $ do
      parseSomething "3.b" expressionP `shouldParseToMD` FunctionApplicationExpression (ReferenceVariable () ("b",(SourcePosition "no_file" 1 3 1 4)) () ()) (NumberLiteral (3 % 1) () () NonEmpty.:| []) () ()

    it "parses telescopes " $ do
      parseSomething "(a : 3, b : a) -> b" expressionP `shouldParseToMD` with_fresh_named "a" (\a -> with_fresh_named "b" (\b -> (TArrowBinding (Scope (NumberLiteral (3 % 1) () ()) (Just (a,np,np) :. Pi (LambdaVariable (a,()) () ()) (Just (b,np,np) :. LambdaVariable (b,()) () ()))) () ())))
    
    it "parses function that takes a number and adds 4" $ do
      parseSomething "|a : Nat| (a + 4)" expressionP `shouldParseToMD` with_fresh_named "a" (\a -> FunctionLiteralExpression (LastArg (ReferenceVariable () ("Nat",(SourcePosition "no_file" 1 6 1 9)) () ()) ((a,NoBind (),NoBind ()) :. ParenthesizedExpression (AddExpression (LambdaVariable (a,()) () ()) (NumberLiteral (4 % 1) () ()) () ()) () ())) () ())

    it "parses function application inline" $ do
      parseSomething "3.(|a : Nat| (a + 4))" expressionP `shouldParseToMD` with_fresh_named "a" (\a -> FunctionApplicationExpression (ParenthesizedExpression (FunctionLiteralExpression (LastArg (ReferenceVariable () ("Nat",(SourcePosition "no_file" 1 9 1 12)) () ()) ((a,NoBind (),NoBind ()) :. ParenthesizedExpression (AddExpression (LambdaVariable (a,()) () ()) (NumberLiteral (4 % 1) () ()) () ()) () ())) () ()) () ()) (NumberLiteral (3 % 1) () () NonEmpty.:| []) () ())

    it "doesn't leak symbols from arrows" $ do
      parseSomething "((a : Type) -> Nat) + a" expressionP `shouldParseToMD`  (AddExpression (ParenthesizedExpression (with_fresh_named "a" (\a -> TArrowBinding (Pi (ReferenceVariable () ("Type",(SourcePosition "no_file" 1 7 1 11)) () ()) (Just (a,NoBind (),NoBind ()) :. ReferenceVariable () ("Nat",SourcePosition {_filePath = "no_file", _sourceLineStart = 1, _sourceColumnStart = 16, _sourceLineEnd = 1, _sourceColumnEnd = 19}) () ())) () ())) () ()) (ReferenceVariable () ("a",(SourcePosition "no_file" 1 23 1 24)) () ()) () ())

    it "doesn't leak symbols from functions" $ do
      parseSomething "(|a : Nat| a) + a" expressionP `shouldParseToMD`  (AddExpression (ParenthesizedExpression (with_fresh_named "a" (\a -> FunctionLiteralExpression (LastArg (ReferenceVariable () ("Nat",(SourcePosition "no_file" 1 7 1 10)) () ()) ((a,NoBind (),NoBind ()) :. LambdaVariable (a,()) () ())) () ())) () ()) (ReferenceVariable () ("a",(SourcePosition "no_file" 1 17 1 18)) () ()) () ())

  describe "Parser.valueDefinitionP" $ do
    it "allows assignment to number literals" $ do
      parseSomething "i = 3" valueDefinitionP `shouldParseTo` ValueDefinition
        "i" (NumberLiteral 3 Untyped (SourcePosition "no_file" 1 5 1 6))
        (SourcePosition "no_file" 1 1 1 6)

        
  describe "Parser.valueDefinitionP" $ do
    it "parses newlines" $ do
      parseSomething "i = \n\
                     \  3 + 4" valueDefinitionP `shouldParseTo` ValueDefinition "i" (AddExpression (NumberLiteral (3 % 1) Untyped (SourcePosition "no_file" 2 3 2 5)) (NumberLiteral (4 % 1) Untyped (SourcePosition "no_file" 2 7 2 8)) Untyped (SourcePosition "no_file" 2 3 2 8)) (SourcePosition "no_file" 1 1 2 8)

  describe "Parser.typeDeclarationP" $ do
    it "allows annotation and assignment to number literals" $ do
      parseSomething "i : 4\ni = 3" valueDefinitionWithAnnotationP `shouldParseTo` ValueDefinitionAnnotated
        "i" (NumberLiteral 3 Untyped (SourcePosition "no_file" 2 5 2 6))
        (SourcePosition "no_file" 2 1 2 6) (NumberLiteral 4 Untyped (SourcePosition "no_file" 1 5 1 6))
        (SourcePosition "no_file" 1 1 1 6)
    
  describe "Parser.parseModule" $ do
    it "parses modules with multiple declarations" $ do 
      let exampleModule = "module i `test module`\n\
                    \test = 3\n\
                    \main = test\n\
                    \" :: Text
      parseModule exampleModule `shouldSatisfy` isRight
      

np = NoBind ()