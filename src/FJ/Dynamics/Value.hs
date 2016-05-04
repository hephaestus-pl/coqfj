module FJ.Dynamics.Value where

import FJ.TypeSystem.Types
import FJ.Syntax.Absfj_syntax

type FieldBindings = [(Id, Instance)]

data Instance = ClassInstance {
	vName :: ClassName,
	fieldBindings :: FieldBindings
}deriving(Eq, Show, Ord)

type Env = [(Var, Instance)]
