{-
Module      :  FJ.TypeSystem.Lookup_functions
Description :  Class lookup auxiliar functions
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  abreu223@hotmail.com & daniaangelos@gmail.com
Stability   :  experimental 
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
Class Lookup auxiliar functions 
field_lookup to retrieve the fields of a class given its ClassName and a CT
method_body  to retrieve the method body of a class given its Name, ClassName and a CT
-}


module FJ.TypeSystem.Lookup_functions where


import FJ.TypeSystem.Types
import FJ.Syntax.Absfj_syntax

classId :: ClassDecl -> Id
classId (CDecl id _ _ _ _) = id

ctEntry :: ClassDecl -> CTEntry
ctEntry cdecl = (classId cdecl, cdecl)

classEntries [] = ClassTable []  
classEntries cdeclList = ClassTable $ map ctEntry cdeclList

programCT :: Program -> ClassTable
programCT (CProgram ct _) = (classEntries ct)

classFields :: ClassDecl -> [FieldDecl]
classFields (CDecl _ _ fieldsDecl _ _) = fieldsDecl

className :: ClassDecl -> String
className (CDecl (Id name) _ _ _ _) = name

superclassOf :: ClassDecl -> ClassName
superclassOf (CDecl _ superclass _ _ _ ) = superclass

classCompare :: ClassName -> CTEntry -> Bool
classCompare cname cte = 
  case cname of ClassObject -> False
                ClassId id -> id == fst cte
  
findClass :: ClassName -> ClassTable -> ClassDecl
findClass cname (ClassTable ct) = snd (head (filter (classCompare cname) ct))

findMethod :: Id -> [MethodDecl] -> MethodDecl
findMethod mname (m@(MethodDecl _ id _ _):ms) =
  if mname == id
  	then m
  	else findMethod mname ms

methodFormalArgs :: MethodDecl -> [FormalArg]
methodFormalArgs (MethodDecl _ _ fl _) = fl

methodTerm :: MethodDecl -> Term
methodTerm (MethodDecl _ _ _ t) = t

mbodyof :: Id -> ClassDecl -> ([FormalArg], Term)
mbodyof mname (CDecl _ _ _ _ mlist) = (formalargs, term) where
  formalargs = methodFormalArgs (findMethod mname mlist)
  term = methodTerm (findMethod mname mlist)

mbody :: Id -> ClassName -> ClassTable -> ([FormalArg], Term)
mbody mname cname ct = mbodyof mname (findClass cname ct)

fargType :: FormalArg -> Type
fargType (FormalArg cn _) = CType cn


fargsToType :: [FormalArg] -> ClassName -> Type
fargsToType [] cn = CType cn
fargsToType (FormalArg farType _ :xs) cd = FType (CType farType) $ fargsToType xs cd

--This function takes a Method name, the Class Name and a CT 
mtype :: MethodDecl -> ClassDecl -> Type
mtype m@(MethodDecl returnType mname _ t) (CDecl cname _ _ _ mlist) = 
    if m `elem` mlist
        then fargsToType (methodFormalArgs m) returnType
        else CType ClassObject


test_prog = CProgram [CDecl (Id "teste") ClassObject [FDecl ClassObject (Id "a")] (KDecl (Id "teste") [Field ClassObject (Id "a")] [] [Assignment (Id "a") (Id "a")]) [], CDecl (Id "teste2") (ClassId $ Id "teste") [] (KDecl (Id "teste2") [] [] []) []] (NewExp (Id "teste") [])

test_prog2 = CProgram [CDecl (Id "A") ClassObject [] (KDecl (Id "A") [] [] []) [],CDecl (Id "B") ClassObject [] (KDecl (Id "B") [] [] []) [],CDecl (Id "Pair") ClassObject [FDecl ClassObject (Id "fst"),FDecl ClassObject (Id "snd")] (KDecl (Id "Pair") [Field ClassObject (Id "fst"),Field ClassObject (Id "snd")] [] [Assignment (Id "fst") (Id "fst"),Assignment (Id "snd") (Id "snd")]) [MethodDecl (ClassId (Id "Pair")) (Id "setfst") [FormalArg ClassObject (Id "newfst"),FormalArg ClassObject (Id "newsnd")] (TermExp (NewExp (Id "Pair") [TermVar (Id "newfst"),TermVar (Id "newsnd")]))]] (NewExp (Id "teste") [])

test_progCT = programCT test_prog
test_prog2CT = programCT test_prog2
