module FJ.TypeSystem.Types where

import FJ.Syntax.Absfj_syntax

type CTEntry = (Id, ClassDecl)

newtype ClassTable = ClassTable [CTEntry]
  deriving (Eq,Ord,Show)

type a :~> b = FType a b

data Type = 
    CType ClassName
  | forall a b. a :~> b 
  deriving (Eq,Ord,Show)



