module FJ.Dynamics.Value where

import FJ.TypeSystem.Types
import FJ.Syntax.Absfj_syntax

type Env = [(Id, Exp)]

data Value  = ClassInstance {
	vName :: ClassName,
	state :: Env
}deriving(Eq, Show, Ord)

newObj = ClassInstance (ClassId (Id "obj")) []

example_ct = ClassTable [(Id "A", CDecl (Id "A") ClassObject [] (KDecl (Id "A") [] [] []) []), (Id "B", CDecl (Id "B") ClassObject [] (KDecl (Id "B") [] [] []) []), (Id "Pair", CDecl (Id "Pair") ClassObject [FDecl (ClassId (Id "A")) (Id "fst"),FDecl (ClassId (Id "B")) (Id "snd")] (KDecl (Id "Pair") [Field (ClassId (Id "A")) (Id "fst"),Field (ClassId (Id "B")) (Id "snd")] [] [Assgnmt (Id "fst") (Id "fst"),Assgnmt (Id "snd") (Id "snd")]) [MDecl (ClassId (Id "Pair")) (Id "setfst") [FArg (ClassId (Id "A")) (Id "newfst")] (NewExp (ClassId (Id "Pair")) [ExpVar (Id "newfst"),ExpFieldAccess ThisAccess (Id "snd")])])]



