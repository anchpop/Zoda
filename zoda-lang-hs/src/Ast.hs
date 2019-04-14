module Ast where

data Module p = Module (ModuleHeader p) [(Declaration p)] p deriving (Show, Read, Eq)
data ModuleHeader p = ModuleHeader (LowercaseIdentifier p) (Tinydoc p) p deriving (Show, Read, Eq)

data Declaration p = Declaration (LowercaseIdentifier p) (Expression p) p deriving (Show, Read, Eq)

data Expression p = Expression (NumberLiteral p) p deriving (Show, Read, Eq)

data NumberLiteral p = NumberLiteral Bool {- - or + -} Int {- . -} Int p deriving (Show, Read, Eq)
data Tinydoc p = Tinydoc String p deriving (Show, Read, Eq)
data LowercaseIdentifier p = LowercaseIdentifier String p deriving (Show, Read, Eq)
data UppercaseIdentifier p = UppercaseIdentifier String p deriving (Show, Read, Eq)
