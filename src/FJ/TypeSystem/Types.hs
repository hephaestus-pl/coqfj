module FJ.TypeSystem.Types where

import FJ.Syntax.Absfj_syntax

type CTEntry = (Id, ClassDecl)

type ClassTable = [CTEntry]
--  deriving (Eq,Ord,Show)

data Type = 
    CType ExpType
  | Type :~>: Type 
  deriving (Eq,Ord,Show)

type ExpType = ClassName

newtype Bind = Bind (Var, Exp)
type Gamma = [TypeBind] 
newtype TypeBind = TypeBind (Var, ExpType)

--instance Monad Type where
--    return a = CType a
--    m >>= f = case m of
--            CType a -> return a
--            a :~>: b -> (a >>= f) :~> (b >>= f)
