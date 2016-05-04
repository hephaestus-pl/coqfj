module FJ.TypeSystem.Types where

import Control.Monad(liftM, ap) 

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

-- Note: in more recent versions of GHC, every 
-- monad instance must also be declared as an 
-- instance of both Functor and Applicative 
-- type classes. Sure, this is a little boring. 
 
instance Functor Result where
	 fmap = liftM

instance Applicative Result where 
	 pure = return
	 (<*>) = ap