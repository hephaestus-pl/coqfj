module FJ.Dynamics.Value where

import FJ.TypeSystem.Types
import FJ.Syntax.Absfj_syntax

data Value  = Object {
	vName :: ClassName,
	state :: [(Id, Value)]
}

