module FJ.TypeSystem.Types where

import FJ.Syntax.Absfj_syntax

type CTEntry = (Id, ClassDecl)

newtype ClassTable = ClassTable [CTEntry]
  deriving (Eq,Ord,Show)

data Type = 
    CType ClassName
  | Type :~>: Type 
  deriving (Eq,Ord,Show)



