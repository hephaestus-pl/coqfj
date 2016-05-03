module FJ.Dynamics.Value where

import FJ.TypeSystem.Types
import FJ.Syntax.Absfj_syntax

type Env = [(Id, Exp)]

data Value  = ClassInstance {
	vName :: ClassName,
	state :: Env
}deriving(Eq, Show, Ord)

newObj = ClassInstance (ClassId (Id "obj")) []




