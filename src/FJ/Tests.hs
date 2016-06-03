module FJ.Tests where 

import FJ.Syntax.Absfj_syntax
import FJ.Dynamics.Computation
import FJ.TypeSystem.Types
import FJ.Syntax.LookupFunctions
import FJ.TypeSystem.TypeChecker
import Test.HUnit
import FJ.Core.CommonTypes

main = runTestTT tests
tests = TestList 
    [TestLabel "Var Type" test1, TestLabel "Field Type" test2
    ,TestLabel "New Type" test3, TestLabel "Method Invoc Type" test4
    ,TestLabel "New Comp" test5, TestLabel "MethodInvoc Comp" test6
    ,TestLabel "Field Comp" test7, TestLabel "1+2 Comp" test8
    ,TestLabel "Add Type" test9, TestLabel "MType ok in" test10]


test1 = TestCase (assertEqual "Var Type" (Ok (ClassId (Id "B"))) var_type)
test2 = TestCase (assertEqual "Field Type" (Ok (ClassId (Id "A"))) field_type)
test3 = TestCase (assertEqual "New Type" (Ok (ClassId (Id "Pair"))) new_type)
test4 = TestCase (assertEqual "Method Invoc Type" (Ok (ClassId (Id "Pair"))) method_invoc_type)

test5 = TestCase (assertEqual "New Comp" 
   (Ok (ExpNew (Id "Pair") [ExpNew (Id "A") [],ExpNew (Id "B") []]))
   new_ex)
test6 = TestCase (assertEqual "Method Invoc Comp" 
   (Ok (ExpNew (Id "Pair") [ExpNew (Id "A") [],ExpFieldAccess (ExpNew (Id "Pair")[ExpNew (Id "A") [],ExpNew (Id "B") []]) (Id "snd")]))
   method_invoc_ex)
test7 = TestCase (assertEqual "Field Comp" (Ok (ExpNew (Id "A") [])) field_acess_ex)
test8 = TestCase (assertEqual "1 + 2 Comp" (Ok (ExpNew (Id "S") [ExpNew (Id "S") [ExpNew (Id "S") [ExpNew (Id "O") []]]]))
    add_ex)
test9 = TestCase (assertEqual "1 + 2 Type" (Ok (ClassId (Id "S"))) add_type)
test10 = TestCase (assertEqual "MType Test" (Ok ()) m_type)

var_type  = expType (ExpVar $ VarId (Id "x")) [TypeBind (VarId (Id "x"), ClassId (Id "B"))] []
field_type  = expType (ExpFieldAccess (ExpNew ( (Id "Pair")) [(ExpNew ( (Id "A")) []), (ExpNew ( (Id "B")) [])]) (Id "fst")) [] example_ct
new_type = expType (ExpNew ( (Id "Pair")) [(ExpNew ( (Id "A")) []), (ExpNew ( (Id "B")) [])]) [] example_ct 
method_invoc_type = expType (ExpMethodInvoc (ExpNew ( (Id "Pair")) [(ExpNew ( (Id "A")) []), (ExpNew ( (Id "B")) [])]) (Id "setfst") [ExpNew ( (Id "A")) []]) [] example_ct 
add_type = expType (programExp int_prog) [] int_ct 

m_type = ((MDecl (ClassId (Id "Pair")) (Id "setfst") [FArg (ClassId (Id "A")) (Id "newfst")] (ExpNew ( (Id "Pair")) [ExpVar $ VarId (Id "newfst"), ExpFieldAccess (ExpVar This) (Id "snd")])) `mOkIn` (ClassId $ Id "Pair")) example_ct 

c_type = classOk (CDecl (Id "A") ClassObject [] (KDecl (Id "B") [] [] []) []) example_ct

classPairOk = classOk (CDecl (Id "Pair") ClassObject [FDecl (ClassId (Id "A")) (Id "fst"),FDecl (ClassId (Id "B")) (Id "snd")] (KDecl (Id "Pair") [FArg (ClassId (Id "A")) (Id "fst"),FArg (ClassId (Id "B")) (Id "snd")] [] [Assgnmt (Id "fst") (Id "fst"),Assgnmt (Id "snd") (Id "snd")]) [MDecl (ClassId (Id "Pair")) (Id "setfst") [FArg (ClassId (Id "A")) (Id "newfst")] (ExpNew ( (Id "Pair")) [ExpVar $ VarId (Id "newfst"), ExpFieldAccess (ExpVar This) (Id "snd")])]) example_ct

new_ex = computation (ExpNew ( (Id "Pair")) [(ExpNew ( (Id "A")) []), (ExpNew ( (Id "B")) [])]) example_ct 
method_invoc_ex = computation (ExpMethodInvoc (ExpNew ( (Id "Pair")) [(ExpNew ( (Id "A")) []), (ExpNew ( (Id "B")) [])]) (Id "setfst") [ExpNew ( (Id "A")) []]) example_ct 
field_acess_ex = computation (ExpFieldAccess (ExpNew ( (Id "Pair")) [(ExpNew ( (Id "A")) []), (ExpNew ( (Id "B")) [])]) (Id "fst")) example_ct 
add_ex = computation (programExp int_prog) int_ct 

example_ct = [(CDecl (Id "A") ClassObject [] (KDecl (Id "B") [] [] []) []), (CDecl (Id "B") ClassObject [] (KDecl (Id "B") [] [] []) []), (CDecl (Id "Pair") ClassObject [FDecl (ClassId (Id "A")) (Id "fst"),FDecl (ClassId (Id "B")) (Id "snd")] (KDecl (Id "Pair") [FArg (ClassId (Id "A")) (Id "fst"),FArg (ClassId (Id "B")) (Id "snd")] [] [Assgnmt (Id "fst") (Id "fst"),Assgnmt (Id "snd") (Id "snd")]) [MDecl (ClassId (Id "Pair")) (Id "setfst") [FArg (ClassId (Id "A")) (Id "newfst")] (ExpNew ( (Id "Pair")) [ExpVar $ VarId (Id "newfst"), ExpFieldAccess (ExpVar This) (Id "snd")])])]

test_prog = CProgram [CDecl (Id "teste") ClassObject [FDecl ClassObject (Id "a")] (KDecl (Id "teste") [FArg ClassObject (Id "a")] [] [Assgnmt (Id "a") (Id "a")]) [], CDecl (Id "teste2") (ClassId $ Id "teste") [] (KDecl (Id "teste2") [] [] []) []] (ExpNew ( Id "teste") [])

test_prog2 = CProgram [CDecl (Id "A") ClassObject [] (KDecl (Id "A") [] [] []) [],CDecl (Id "B") ClassObject [] (KDecl (Id "B") [] [] []) [],CDecl (Id "Pair") ClassObject [FDecl (ClassId (Id "A")) (Id "fst"),FDecl (ClassId (Id "B")) (Id "snd")] (KDecl (Id "Pair") [FArg (ClassId (Id "A")) (Id "fst"),FArg (ClassId (Id "B")) (Id "snd")] [] [Assgnmt (Id "fst") (Id "fst"),Assgnmt (Id "snd") (Id "snd")]) [MDecl (ClassId (Id "Pair")) (Id "setfst") [FArg (ClassId (Id "A")) (Id "newfst")] (ExpNew ( (Id "Pair")) [ExpVar $ VarId (Id "newfst"), ExpNew ( (Id "B")) []])]] (ExpNew ( (Id "Pair")) [ExpNew ( (Id "A")) [],ExpNew ( (Id "B")) []])

int_prog = CProgram [CDecl (Id "Int") ClassObject [] (KDecl (Id "Int") [] [] []) [MDecl (ClassId (Id "Int")) (Id "add") [FArg (ClassId (Id "Int")) (Id "rhs")] (ExpMethodInvoc (ExpVar (VarId (Id "rhs"))) (Id "add") [ExpVar This])],CDecl (Id "O") (ClassId (Id "Int")) [] (KDecl (Id "O") [] [] []) [MDecl (ClassId (Id "Int")) (Id "add") [FArg (ClassId (Id "Int")) (Id "rhs")] (ExpVar (VarId (Id "rhs")))],CDecl (Id "S") (ClassId (Id "Int")) [FDecl (ClassId (Id "Int")) (Id "num")] (KDecl (Id "S") [FArg (ClassId (Id "Int")) (Id "num")] [] [Assgnmt (Id "num") (Id "num")]) [MDecl (ClassId (Id "Int")) (Id "add") [FArg (ClassId (Id "Int")) (Id "rhs")] (ExpMethodInvoc (ExpFieldAccess (ExpVar This) (Id "num")) (Id "add") [ExpNew (Id "S") [ExpVar (VarId (Id "rhs"))]])]] (ExpMethodInvoc (ExpNew (Id "S") [ExpNew (Id "S") [ExpNew (Id "O") []]]) (Id "add") [ExpNew (Id "S") [ExpNew (Id "O") []]])


--test_progCT = programCT test_prog
--test_prog2CT = programCT test_prog2
--a_class = findClass (ClassId $ Id "A") test_prog2CT
--pair_class = findClass (ClassId $ Id "Pair") test_prog2CT
--setfst_body = methodDecl (Id "setfst") pair_class

--setfst_type = methodType (Id "setfst") (ClassId $ Id "Pair") test_prog2CT
int_ct = programCT int_prog


