{-
Module      :  FJ.TypeSystem.Lookup_functions
Description :  Class lookup auxiliar functions
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  abreu223@hotmail.com & daniangelos@hotmail.com
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

classEntries :: [ClassDecl] -> ClassTable
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

-- fields :: ClassTable -> ClassName -> [FieldDecl]


{-
transFD_Field :: [FD] -> [Field]
transFD_Field [] = []
transFD_Field ((FDecl t i):xs) = (Field t i):transFD_Field xs


field_lookup' :: [CD] -> [CD] -> ClassName -> [Field]
field_lookup' _ _ TypeObject = []
field_lookup' [] _ _ = []
field_lookup' ct@(CDecl id parent_class fields  _ _ : xs) consumed (TypeId id') = 
            if id == id' then (transFD_Field fields) ++ (field_lookup (ct++consumed) parent_class)
            else field_lookup' xs (x:consumed) (TypeId id') 
  where x = head ct

field_lookup :: [CD] -> Type -> [Field]
field_lookup ct ty = field_lookup' ct [] ty
-}

test_prog = CProgram [CDecl (Id "teste") ClassObject [FDecl ClassObject (Id "a")] (KDecl (Id "teste") [Field ClassObject (Id "a")] [] [Assignment (Id "a") (Id "a")]) [], CDecl (Id "teste2") (ClassId $ Id "teste") [] (KDecl (Id "teste2") [] [] []) []] (NewExp (Id "teste") [])

test_progCT = programCT test_prog

