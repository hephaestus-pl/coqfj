module FJ.Dynamics.Value where

import FJ.TypeSystem.Types
import FJ.Syntax.Absfj_syntax

type FieldBindings = [(Id, Value)]

data Value = ClassInstance {
	vName :: ClassName,
	fieldBindings :: FieldBindings
}deriving(Eq, Show, Ord)

type Env = [(Var, Value)]
