module FJ.TypeSystem.Types where

import FJ.Syntax.Absfj_syntax

type CTEntry = (Id, ClassDecl)

newtype ClassTable = ClassTable [CTEntry]
  deriving (Eq,Ord,Show)

data Type = 
    CType ClassName
  | FType Type Type 
  deriving (Eq,Ord,Show)

-- data a :~> b = FType a b
