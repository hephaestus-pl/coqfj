
cast_ex = eval example_ct (NewExp (ClassId (Id "Pair")) [(NewExp (ClassId (Id "A")) []), (NewExp (ClassId (Id "B")) [])]) 
method_invoc_ex = eval test_prog2CT (ExpMethodInvoc $ NewExp (ClassId (Id "Pair")) [(NewExp (ClassId (Id "A")) []), (NewExp (ClassId (Id "B")) [])]) (Id "setfst") [NewExp (ClassId (Id "A")) []]) 
field_acess_ex = eval example_ct (ExpFieldAccess $ NewExp (ClassId (Id "Pair")) [(NewExp (ClassId (Id "A")) []), (NewExp (ClassId (Id "B")) [])]) (Id "snd")) 

example_ct = ClassTable [(Id "A", CDecl (Id "A") ClassObject [] (KDecl (Id "A") [] [] []) []), (Id "B", CDecl (Id "B") ClassObject [] (KDecl (Id "B") [] [] []) []), (Id "Pair", CDecl (Id "Pair") ClassObject [FDecl (ClassId (Id "A")) (Id "fst"),FDecl (ClassId (Id "B")) (Id "snd")] (KDecl (Id "Pair") [Field (ClassId (Id "A")) (Id "fst"),Field (ClassId (Id "B")) (Id "snd")] [] [Assgnmt (Id "fst") (Id "fst"),Assgnmt (Id "snd") (Id "snd")]) [MDecl (ClassId (Id "Pair")) (Id "setfst") [FArg (ClassId (Id "A")) (Id "newfst")] (NewExp (ClassId (Id "Pair")) [ExpVar (Id "newfst"),ExpFieldAccess ThisExp (Id "snd")])])]
