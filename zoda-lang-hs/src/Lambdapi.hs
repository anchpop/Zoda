module Lambdapi where

import Prelude hiding (print)

data ITerm = Ann    CTerm Type
           | Bound  Int
           | Free   Name
           | ITerm :@: CTerm
           deriving (Show, Eq)
 
data CTerm = Inf  ITerm 
           | Lam  CTerm
           deriving (Show, Eq)

data Name = Global  String
          | Local   Int
          | Quote   Int
          deriving (Show, Eq)
    
data Type = TFree  Name
          | Fun    Type Type
          deriving (Show, Eq)

data Value = VLam      (Value -> Value)
           | VNeutral  Neutral

data Neutral = NFree  Name
             | NApp   Neutral Value

type Env = [Value]
    
data Kind = Star deriving (Show)

data Info = HasKind  Kind
          | HasType  Type deriving (Show)

type Context = [(Name, Info)]

vfree :: Name -> Value
vfree n = VNeutral (NFree n)

iEval :: ITerm -> Env -> Value
iEval (Ann  e _)    d  =  cEval e d
iEval (Free  x)     d  =  vfree x--case lookup x (fst d) of Nothing ->  (vfree x); Just v -> v
iEval (Bound  ii)   d  =  d !! ii --(snd d) !! ii
iEval (e1 :@: e2)   d  =  vapp (iEval e1 d) (cEval e2 d)

vapp :: Value -> Value -> Value
vapp (VLam f)      v  =  f v
vapp (VNeutral n)  v  =  VNeutral (NApp n v)

cEval :: CTerm -> Env -> Value
cEval (Inf  ii)   d  =  iEval ii d
cEval (Lam  e)    d  =  VLam (\x -> cEval e (x : d)) --VLam (\x -> cEval e (((\(e, d) -> (e,  (x : d))) d)))

type Result α = Either String α

