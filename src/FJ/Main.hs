module FJ.Main where 

import FJ.Syntax.Absfj_syntax
import FJ.Dynamics.Interpreter
import FJ.TypeSystem.Types
import FJ.TypeSystem.Lookup_functions

cast_ex = eval example_ct (NewExp (ClassId (Id "Pair")) [(NewExp (ClassId (Id "A")) []), (NewExp (ClassId (Id "B")) [])]) 
method_invoc_ex = eval example_ct (ExpMethodInvoc (NewExp (ClassId (Id "Pair")) [(NewExp (ClassId (Id "A")) []), (NewExp (ClassId (Id "B")) [])]) (Id "setfst") [NewExp (ClassId (Id "A")) []]) 
field_acess_ex = eval example_ct (ExpFieldAccess (NewExp (ClassId (Id "Pair")) [(NewExp (ClassId (Id "A")) []), (NewExp (ClassId (Id "B")) [])]) (Id "fst")) 

example_ct = ClassTable [(Id "A", CDecl (Id "A") ClassObject [] (KDecl (Id "A") [] [] []) []), (Id "B", CDecl (Id "B") ClassObject [] (KDecl (Id "B") [] [] []) []), (Id "Pair", CDecl (Id "Pair") ClassObject [FDecl (ClassId (Id "A")) (Id "fst"),FDecl (ClassId (Id "B")) (Id "snd")] (KDecl (Id "Pair") [Field (ClassId (Id "A")) (Id "fst"),Field (ClassId (Id "B")) (Id "snd")] [] [Assgnmt (Id "fst") (Id "fst"),Assgnmt (Id "snd") (Id "snd")]) [MDecl (ClassId (Id "Pair")) (Id "setfst") [FArg (ClassId (Id "A")) (Id "newfst")] (NewExp (ClassId (Id "Pair")) [ExpVar $ IdVar (Id "newfst"), ExpFieldAccess (ExpVar This) (Id "snd")])])]

test_prog = CProgram [CDecl (Id "teste") ClassObject [FDecl ClassObject (Id "a")] (KDecl (Id "teste") [Field ClassObject (Id "a")] [] [Assgnmt (Id "a") (Id "a")]) [], CDecl (Id "teste2") (ClassId $ Id "teste") [] (KDecl (Id "teste2") [] [] []) []] (NewExp (ClassId $ Id "teste") [])

test_prog2 = CProgram [CDecl (Id "A") ClassObject [] (KDecl (Id "A") [] [] []) [],CDecl (Id "B") ClassObject [] (KDecl (Id "B") [] [] []) [],CDecl (Id "Pair") ClassObject [FDecl (ClassId (Id "A")) (Id "fst"),FDecl (ClassId (Id "B")) (Id "snd")] (KDecl (Id "Pair") [Field (ClassId (Id "A")) (Id "fst"),Field (ClassId (Id "B")) (Id "snd")] [] [Assgnmt (Id "fst") (Id "fst"),Assgnmt (Id "snd") (Id "snd")]) [MDecl (ClassId (Id "Pair")) (Id "setfst") [FArg (ClassId (Id "A")) (Id "newfst")] (NewExp (ClassId (Id "Pair")) [ExpVar $ IdVar (Id "newfst"), NewExp (ClassId (Id "B")) []])]] (NewExp (ClassId (Id "Pair")) [NewExp (ClassId (Id "A")) [],NewExp (ClassId (Id "B")) []])

test_progCT = programCT test_prog
test_prog2CT = programCT test_prog2
a_class = findClass (ClassId $ Id "A") test_prog2CT
--pair_class = findClass (ClassId $ Id "Pair") test_prog2CT
--setfst_body = methodDecl (Id "setfst") pair_class

setfst_type = methodType (Id "setfst") (ClassId $ Id "Pair") test_prog2CT

