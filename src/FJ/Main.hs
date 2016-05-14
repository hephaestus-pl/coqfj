module FJ.Main where 

import FJ.Syntax.Absfj_syntax
import FJ.Dynamics.Computation
import FJ.TypeSystem.Types
import FJ.Syntax.LookupFunctions
import FJ.TypeSystem.TypeChecker

var_type  = expType (ExpVar $ VarId (Id "x")) [TypeBind (VarId (Id "x"), ClassObject)] []
field_type  = expType (ExpFieldAccess (ExpNew ( (Id "Pair")) [(ExpNew ( (Id "A")) []), (ExpNew ( (Id "B")) [])]) (Id "fst")) []example_ct

cast_ex = computation (ExpNew ( (Id "Pair")) [(ExpNew ( (Id "A")) []), (ExpNew ( (Id "B")) [])]) example_ct 
method_invoc_ex = computation (ExpMethodInvoc (ExpNew ( (Id "Pair")) [(ExpNew ( (Id "A")) []), (ExpNew ( (Id "B")) [])]) (Id "setfst") [ExpNew ( (Id "A")) []]) example_ct 
field_acess_ex = computation (ExpFieldAccess (ExpNew ( (Id "Pair")) [(ExpNew ( (Id "A")) []), (ExpNew ( (Id "B")) [])]) (Id "fst")) example_ct 
add_ex = computation (programExp int_prog) int_ct 

example_ct = [(Id "A", CDecl (Id "A") ClassObject [] (KDecl (Id "A") [] [] []) []), (Id "B", CDecl (Id "B") ClassObject [] (KDecl (Id "B") [] [] []) []), (Id "Pair", CDecl (Id "Pair") ClassObject [FDecl (ClassId (Id "A")) (Id "fst"),FDecl (ClassId (Id "B")) (Id "snd")] (KDecl (Id "Pair") [Field (ClassId (Id "A")) (Id "fst"),Field (ClassId (Id "B")) (Id "snd")] [] [Assgnmt (Id "fst") (Id "fst"),Assgnmt (Id "snd") (Id "snd")]) [MDecl (ClassId (Id "Pair")) (Id "setfst") [FArg (ClassId (Id "A")) (Id "newfst")] (ExpNew ( (Id "Pair")) [ExpVar $ VarId (Id "newfst"), ExpFieldAccess (ExpVar This) (Id "snd")])])]

test_prog = CProgram [CDecl (Id "teste") ClassObject [FDecl ClassObject (Id "a")] (KDecl (Id "teste") [Field ClassObject (Id "a")] [] [Assgnmt (Id "a") (Id "a")]) [], CDecl (Id "teste2") (ClassId $ Id "teste") [] (KDecl (Id "teste2") [] [] []) []] (ExpNew ( Id "teste") [])

test_prog2 = CProgram [CDecl (Id "A") ClassObject [] (KDecl (Id "A") [] [] []) [],CDecl (Id "B") ClassObject [] (KDecl (Id "B") [] [] []) [],CDecl (Id "Pair") ClassObject [FDecl (ClassId (Id "A")) (Id "fst"),FDecl (ClassId (Id "B")) (Id "snd")] (KDecl (Id "Pair") [Field (ClassId (Id "A")) (Id "fst"),Field (ClassId (Id "B")) (Id "snd")] [] [Assgnmt (Id "fst") (Id "fst"),Assgnmt (Id "snd") (Id "snd")]) [MDecl (ClassId (Id "Pair")) (Id "setfst") [FArg (ClassId (Id "A")) (Id "newfst")] (ExpNew ( (Id "Pair")) [ExpVar $ VarId (Id "newfst"), ExpNew ( (Id "B")) []])]] (ExpNew ( (Id "Pair")) [ExpNew ( (Id "A")) [],ExpNew ( (Id "B")) []])

int_prog = CProgram [CDecl (Id "Int") ClassObject [] (KDecl (Id "Int") [] [] []) [],CDecl (Id "O") (ClassId (Id "Int")) [] (KDecl (Id "O") [] [] []) [MDecl (ClassId (Id "Int")) (Id "add") [FArg (ClassId (Id "Int")) (Id "rhs")] (ExpVar (VarId (Id "rhs")))],CDecl (Id "S") (ClassId (Id "Int")) [FDecl (ClassId (Id "Int")) (Id "num")] (KDecl (Id "S") [Field (ClassId (Id "Int")) (Id "num")] [] [Assgnmt (Id "num") (Id "num")]) [MDecl (ClassId (Id "Int")) (Id "add") [FArg (ClassId (Id "Int")) (Id "rhs")] (ExpMethodInvoc (ExpFieldAccess (ExpVar This) (Id "num")) (Id "add") [ExpNew ((Id "S")) [ExpVar (VarId (Id "rhs"))]])]] (ExpMethodInvoc (ExpNew ( (Id "S")) [ExpNew ( (Id "S")) [ExpNew ( (Id "O")) []]]) (Id "add") [ExpNew ( (Id "S")) [ExpNew ( (Id "O")) []]])


--test_progCT = programCT test_prog
--test_prog2CT = programCT test_prog2
--a_class = findClass (ClassId $ Id "A") test_prog2CT
--pair_class = findClass (ClassId $ Id "Pair") test_prog2CT
--setfst_body = methodDecl (Id "setfst") pair_class

--setfst_type = methodType (Id "setfst") (ClassId $ Id "Pair") test_prog2CT
int_ct = programCT int_prog


