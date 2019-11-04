module Parser where
import ClassyPrelude hiding (try, many)
import Ast
import Basic

import Text.Megaparsec hiding (State, some)
import Text.Megaparsec.Char
import Control.Monad.State
import Capability.Error

import qualified Data.List.NonEmpty as NonEmpty
import Data.Ratio
import qualified Data.Bifunctor as Data.Bifunctor
import qualified Data.Set as Set
import qualified Data.List as List

import Text.Megaparsec.Debug
import Nominal hiding ((.))

parseModule :: (HasThrow "perr" (ProductionError Untyped p () Text) m) => String -> m (Module Untyped SourcePosition () Text)
parseModule text = handleResult result
 where
  result = undefined --Data.Bifunctor.first ZodaSyntaxError (runParser (evalStateT moduleP []) "module" text)
  handleResult (Left  e) = throw @"perr" e
  handleResult (Right r) = pure r

moduleP :: ASTParser Module
moduleP = sourcePosWrapper $ do
  header       <- moduleHeaderP
  declarations <- many declarationP
  pure (Module header declarations)


moduleHeaderP :: ASTParser ModuleHeader
moduleHeaderP = sourcePosWrapperWithNewlines $ do
  string "module"
  some separatorChar
  ident <- identifierP
  some separatorChar
  doc <- tinydocP
  pure (ModuleHeader ident doc)


declarationP :: ASTParser Declaration
declarationP = sourcePosWrapperWithNewlines $ do
  ident <- identifierP
  some separatorChar
  char '='
  some separatorChar
  expression <- expressionP
  pure (Declaration ident expression)


expressionP :: ASTParser Expression
expressionP = allWithModifier annotationWrapper [allWithModifier tarrowWrapper1 [],  allWithModifier tarrowWrapper2 []] <|> allWithModifier funcAppWrapper [] <|> allWithModifier tarrowWrapper1 [] <|> allWithModifier tarrowWrapper2 [] <|> parenthesizedExpression <|> numb <|> ident <|> fliteral
 where
  allWithModifier f xs = foldr (<|>) (f parenthesizedExpression) (map f $ [fliteral, ident, numb] <> xs)
  annotationWrapper expType = sourcePosWrapper . try $ do
    expr1 <- expType
    many separatorChar
    string ":"
    many separatorChar
    expr2 <- expressionP
    pure (Annotation expr1 expr2 Untyped) 
  tarrowWrapper1 expType = sourcePosWrapper . try $ do
    expr1 <- expType
    many separatorChar
    string "->"
    many separatorChar
    expr2 <- expressionP
    pure (TArrow Nothing expr1 expr2 Untyped)
  tarrowWrapper2 expType = sourcePosWrapper . try $ do
    expr1 <- expType
    many separatorChar
    string "("
    many separatorChar
    ident <- ident
    many separatorChar
    string ")"
    many separatorChar
    string "->"
    many separatorChar
    expr2 <- expressionP
    pure (TArrow (Just ident) expr1 expr2 Untyped)
  funcAppWrapper expType = sourcePosWrapper . try . (fmap (\(f, applicants) -> FunctionApplicationExpression f applicants Untyped)) $ typedotexp
    where
      typedotexp = do
        applicant <- expType
        string "."
        f <- expressionP
        pure (f, [applicant])

  parenthesizedExpression = sourcePosWrapper . try $ do
    char '('
    many separatorChar
    exp <- expressionP
    many separatorChar
    char ')'
    pure (ParenthesizedExpression exp Untyped)

  numb = sourcePosWrapper . try $ do
    numb <- numberLiteralP
    pure (NumberLiteral (numerator numb) (denominator numb) Untyped)
  ident = identifierP
  fliteral = sourcePosWrapper . try $ do
    flit <- functionLiteralP
    pure undefined --(FunctionLiteralExpression flit Untyped)
  

numberLiteralP :: Parser Rational
numberLiteralP = try
          ( do
            sign   <- try (string "-" *> pure False) <|> (pure True)
            majorS <- some' (digitChar)
            minorS <- (try (char '.') *> some' (digitChar)) <|> (pure $ '0' NonEmpty.:| [])
            guard $ NonEmpty.head majorS /= '0'
            case readMay minorS of
              Just (minor) -> case (readMay ((toList majorS) <> " % 1") :: Maybe Rational) of
                Just (major) -> do
                  let value1 = minor % (10 ^ (length minorS)) + (major) -- % is division, not modulo

                      value2 = if sign then value1 else (negate value1)
                  pure (value2)
                _ -> empty
              _ -> empty
          )
    <|> ( do
          sign   <- try (string "-" *> pure False) <|> (pure True)
          majorS <- some' (digitChar)
          guard $ NonEmpty.head majorS /= '0'
          let major :: Rational
              major = justUnsafe (readMay ((toList majorS) <> " % 1"))
          pure $ if sign then major else negate major
        )
  where justUnsafe (Just x) = x

functionLiteralP :: Parser (Bind [(String, Atom)] (Expression Untyped SourcePosition () Text))
functionLiteralP = do
  char '|'
  identifiers <- identifierP `sepBy1` (char ',' *> some separatorChar)
  char '|'
  let duplicates = List.length (Set.fromList identifiers) < List.length identifiers
  when duplicates (customFailure DuplicateFunctionArgumentNames)
  let binders = map (\name -> with_fresh_named name (\(x :: Atom) -> (name, x))) identifiers
  some separatorChar


  -- string "->"
  -- some separatorChar
  exp <- withEnvInState binders expressionP
  pure $ binders :. exp


withEnvInState e a = do
  s <- get
  put (e:s) 
  val <- a
  put s
  pure val


tinydocP :: ASTParser Tinydoc
tinydocP = sourcePosWrapper $ do
  char '`'
  doc <- some (noNewlineOrChars "`")
  char '`'
  pure . Tinydoc . fromString $ doc


identifierP :: a --Parser (Text, SourcePosition)
identifierP = undefined
  {-sourcePosWrapper
    $ ( do
        c    <- letterChar 
        rest <- many identifierCharacter
        pure . Identifier . fromString $ (c : rest)
      )-}


identifierCharacter :: Parser Char
identifierCharacter = try letterChar <|> try alphaNumChar <|> try (char '\'') <|> try (char '-')

data Errors = DuplicateFunctionArgumentNames deriving (Show, Read, Ord, Eq, NominalSupport, NominalShow, Generic, Nominal)
type ParserState = [[(String, Atom)]]
type Parser a = StateT ParserState (Parsec Errors String) a
type ASTParser a = Parser (a Untyped SourcePosition () Text)
data SourcePosition = SourcePosition {_filePath :: String, _sourceLineStart :: Int, _sourceColumnStart  :: Int, _sourceLineEnd :: Int, _sourceColumnEnd  :: Int} deriving (Read, Eq, NominalSupport, NominalShow, Generic, Nominal, Bindable)
instance Show SourcePosition where
  show (SourcePosition f l1 c1 l2 c2) = "(SourcePosition \"" <> f <> "\" " <> (show l1) <> " " <> (show c1) <> " " <> (show l2) <> " " <> (show c2) <> ")"


sourcePosWrapper :: Parser (SourcePosition -> a) -> Parser a
sourcePosWrapper f = do
  (SourcePos n l1 c1) <- getSourcePos
  toApply             <- f
  (SourcePos _ l2 c2) <- getSourcePos
  pure $ toApply (SourcePosition n (unPos l1) (unPos c1) (unPos l2) (unPos c2))

sourcePosWrapperWithNewlines :: Parser (SourcePosition -> a) -> Parser a
sourcePosWrapperWithNewlines f = sourcePosWrapper f <* (many separatorChar *> some newline *> pure () <|> (lookAhead eof))


some' :: Parser a -> Parser (NonEmpty.NonEmpty a)
some' p = do
  first <- p
  rest  <- many p
  pure $ first NonEmpty.:| rest


getRight :: Either (ParseErrorBundle String a) b -> b
getRight (Right b  ) = b
getRight (Left  err) = error $ undefined --errorBundlePretty err
getRightZSE (Right b  ) = b
getRightZSE (Left  (ZodaSyntaxError err)) = error $ undefined --errorBundlePretty err

justUnsafe (Just a) = a

--parseSomething :: Stream s => s -> StateT ParserState (Parsec e s) a -> Either (ParseErrorBundle s e) a

parseSomething text parser = (runParser (evalStateT (parser <* eof) []) "no_file" text)


noNewlineOrChars :: (MonadParsec e s m, Token s ~ Char) => [Char] -> m (Token s)
noNewlineOrChars c = noneOf ('\n' : c)