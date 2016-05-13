{-
Module      :  FJ.TypeSystem.Lookup_functions
Description :  Class lookup auxiliar functions
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  abreu223@hotmail.com & daniaangelos@gmail.com
Stability   :  experimental 
Portability :  portable

<module description starting at first column>
Class Lookup auxiliar functions 
field_lookup to retrieve the fields of a class given its ClassName and a CT
method_body  to retrieve the method body of a class given its Name, ClassName and a CT
-}


module FJ.Syntax.LookupFunctions where

import Core.CommonTypes 

import FJ.TypeSystem.Types
import FJ.Syntax.Absfj_syntax

createClassTable :: [ClassDecl] -> ClassTable
createClassTable css = [((Id . ref) c, c) | c <- css]

superClassOf :: ClassDecl -> ClassName
superClassOf (CDecl _ s _ _ _ ) = s

classFields :: ClassDecl -> [FieldDecl]
classFields (CDecl _ _ fdls _ _) = fdls

classMethods :: ClassDecl -> [MethodDecl]
classMethods (CDecl _ _ _ _ mdls) = mdls

methodFormalArgs :: MethodDecl -> [FormalArg]
methodFormalArgs (MDecl _ _ args _) = args

methodBody :: MethodDecl -> Exp
methodBody (MDecl _ _ _ body) = body

strToVar :: String -> Var
strToVar str = IdVar (Id str)

fargsToVar :: [FormalArg] -> [Var]
fargsToVar fargs = map strToVar $ map ref fargs

instance Referable ClassDecl where 
  ref (CDecl (Id s) _ _ _ _) = s

instance Referable FieldDecl where 
  ref (FDecl _ (Id s)) = s

instance Referable MethodDecl where 
  ref (MDecl _ (Id s) _ _) = s 

instance Referable ClassName where
  ref (ClassId (Id s)) = s

instance Referable Id where
  ref (Id s) = s

instance Referable FormalArg where
  ref (FArg _ (Id s)) = s

-- test_prog = CProgram [CDecl (Id "teste") ClassObject [FDecl ClassObject (Id "a")] (KDecl (Id "teste") [Field ClassObject (Id "a")] [] [Assgnmt (Id "a") (Id "a")]) [], CDecl (Id "teste2") (ClassId $ Id "teste") [] (KDecl (Id "teste2") [] [] []) []] (NewExp (ClassId $ Id "teste") [])

-- test_prog2 = CProgram [CDecl (Id "A") ClassObject [] (KDecl (Id "A") [] [] []) [],CDecl (Id "B") ClassObject [] (KDecl (Id "B") [] [] []) [],CDecl (Id "Pair") ClassObject [FDecl (ClassId (Id "A")) (Id "fst"),FDecl (ClassId (Id "B")) (Id "snd")] (KDecl (Id "Pair") [Field (ClassId (Id "A")) (Id "fst"),Field (ClassId (Id "B")) (Id "snd")] [] [Assgnmt (Id "fst") (Id "fst"),Assgnmt (Id "snd") (Id "snd")]) [MDecl (ClassId (Id "Pair")) (Id "setfst") [FArg (ClassId (Id "A")) (Id "newfst")] (NewExp (ClassId (Id "Pair")) [ExpVar (Id "newfst"),NewExp (ClassId (Id "B")) []])]] (NewExp (ClassId (Id "Pair")) [NewExp (ClassId (Id "A")) [],NewExp (ClassId (Id "B")) []])

-- test_progCT = programCT test_prog
-- test_prog2CT = programCT test_prog2
-- a_class = findClass (ClassId $ Id "A") test_prog2CT
-- pair_class = findClass (ClassId $ Id "Pair") test_prog2CT
-- setfst_body = methodDecl (Id "setfst") pair_class

-- setfst_type = methodType (Id "setfst") (ClassId $ Id "Pair") test_prog2CT

