module FJ.Dynamics.Value where

import FJ.Dynamics.Environment

import FJ.TypeSystem.Types
import FJ.Syntax.Absfj_syntax

data Value  = ClassInstance {
	vName :: ClassName,
	state :: Env
}deriving(Eq, Show, Ord)

-- newObj = ClassInstance (ClassId (Id "obj")) []




