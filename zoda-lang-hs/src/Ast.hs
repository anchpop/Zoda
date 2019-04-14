module Ast where

data Module p = Module (ModuleHeader p) [(Declaration p)] p deriving (Show, Read, Eq)
data ModuleHeader p = ModuleHeader (Identifier p) (TinyDoc p) p deriving (Show, Read, Eq)

data Declaration p = Declaration (Identifier p) (Expression p) p deriving (Show, Read, Eq)

data Expression p = Expression (NumberLiteral p) p deriving (Show, Read, Eq)

data NumberLiteral p = NumberLiteral Bool {- - or + -} Int {- . -} Int p deriving (Show, Read, Eq)
data TinyDoc p = TinyDoc String p deriving (Show, Read, Eq)
data Identifier p = Identifier String p deriving (Show, Read, Eq)
