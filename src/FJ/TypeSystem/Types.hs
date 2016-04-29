module FJ.TypeSystem.Types where

import FJ.Syntax.Absfj_syntax

type CTEntry = (Id, ClassDecl)

newtype ClassTable = ClassTable [CTEntry]
  deriving (Eq,Ord,Show)

data Type = 
    CType ClassName
  | Type :~>: Type 
  deriving (Eq,Ord,Show)

type Exception = String

data Result a = Ok a
	| Ex Exception
	deriving(Eq, Show, Ord)

raise :: Exception -> Result a
raise x = Ex x 

instance Monad Result where
	return a = Ok a
	{-(>>=) :: MOut a -> (a -> MOut b) -> MOut b-}
	(>>=) m f = case m of 
				(Ok a) -> f a
				(Ex e) -> raise e


