module Core where
corecorecore = corecorecore
{-import ClassyPrelude
import Data.Type.Coercion
import Data.Roles
import Data.Functor.Foldable
import Text.Show.Deriving
import Text.Read.Deriving
import Data.Eq.Deriving
import Data.Ord.Deriving
import Data.Functor.Foldable.TH
import qualified Data.Map.Justified as Map

import qualified Ast

newtype Program ph t p k = Program (Map.Map ph k (Expression ph t p k))

data Expression ph t p k = NumberLiteral                 Rational                                     t p
                         | IdentifierExpression          (Ast.LowercaseIdentifier t p (Map.Key ph k)) t p 
                         | LambdaExpression              (Lambda ph t p k)                            t p 
                         | FunctionApplicationExpression (Expression ph t p k) (Expression ph t p k)  t p 


data Lambda ph t p k = forall ph'. FunctionLiteral ((Map.Key ph' k, Map.Key ph k -> Map.Key ph' k, Map.Map ph' k (Expression ph' t p (Map.Key ph' k))) -> Expression ph t p (Map.Key ph' k)) p 


data Untyped = Untyped deriving (Show, Read, Eq, Ord)

-} 