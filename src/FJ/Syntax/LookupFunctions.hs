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


-- classId :: ClassDecl -> Id
-- classId (CDecl id _ _ _ _) = id

-- ctEntry :: ClassDecl -> CTEntry
-- ctEntry cdecl = (classId cdecl, cdecl)

-- classEntries :: [ClassDecl] -> ClassTable
-- classEntries [] = ClassTable []  
-- classEntries cdeclList = ClassTable $ map ctEntry cdeclList

-- programCT :: Program -> ClassTable
-- programCT (CProgram ct _) = (classEntries ct)

-- perhaps we'll eventually have to refactor this to signature
-- Id -> ClassName -> ClassTable -> [FieldDecl]
-- so we can have ClassObject with nil fields
-- The same idea applies to findClass
-- classFields :: ClassDecl -> [FieldDecl]
-- classFields (CDecl _ _ fieldsDecl _ _) = fieldsDecl

-- fieldId :: FieldDecl -> Id
-- fieldId (FDecl _ id) = id

-- className :: ClassDecl -> String
-- className (CDecl (Id name) _ _ _ _) = name

-- superclassOf :: ClassDecl -> ClassName
-- superclassOf (CDecl _ superclass _ _ _ ) = superclass

-- classCompare :: ClassName -> CTEntry -> Bool
-- classCompare ClassObject _ = False
-- classCompare (ClassId id) cte = id == (fst cte)
  
-- objConstr = KDecl (Id "Object") [] [] []
-- objectCDecl = CDecl (Id "Object") ClassObject [] objConstr []

-- findClass :: ClassName -> ClassTable -> Result ClassDecl
-- findClass ClassObject _ = Ok objectCDecl
-- findClass cname (ClassTable ct) = -- return 
--    case (filter (classCompare cname) ct) of
--     [] -> raise "Could not find Class Declaration"
--     (c:cs) -> Ok $ snd c 

-- findMethod :: Id -> [MethodDecl] -> MethodDecl
-- findMethod mname (m@(MDecl _ id _ _):ms) =
--   if mname == id
--   	then m
--   	else findMethod mname ms


-- mbodyof :: Id -> ClassDecl -> ([FormalArg], Exp)
-- mbodyof mname (CDecl _ _ _ _ mlist) = (formalargs, term) where
--   formalargs = methodFormalArgs (findMethod mname mlist)
--   term = methodExp (findMethod mname mlist)

--mbody :: Id -> ClassName -> ClassTable -> ([FormalArg], Exp)
--mbody mname cname ct = mbodyof mname (findClass cname ct)

-- fargType :: FormalArg -> Type
-- fargType (FArg cn _) = CType cn


-- fargsToType :: [FormalArg] -> ClassName -> Type
-- fargsToType [] cn = CType cn
-- fargsToType (FArg farType _ :xs) cd = CType farType :~>: fargsToType xs cd

--This function takes a Method Name, the Class Name and a CT 
-- mtype' :: MethodDecl -> ClassDecl -> Type
-- mtype' m@(MDecl returnType mname _ t) (CDecl cname _ _ _ mlist) = 
--     if m `elem` mlist
--         then fargsToType (methodFormalArgs m) returnType
--         else CType ClassObject


-- methodId :: MethodDecl -> Id
-- methodId (MDecl _ mname _ _) = mname

-- classMethods :: ClassDecl -> [MethodDecl]
-- classMethods (CDecl _ _ _ _ mdecls) = mdecls

-- methodDecl :: Id -> ClassDecl -> Result MethodDecl
-- methodDecl mname (CDecl _ _ _ _ mdecls) = 
--   case filter (\m -> methodId m == mname) mdecls of
--       [] -> raise $ "Couldn't find method with name " ++ idToString mname
--       x:xs -> Ok x

-- idToString :: Id -> String
-- idToString (Id str) = str

-- cNameToString :: ClassName -> String
-- cNameToString ClassObject = "Object"
-- cNameToString (ClassId (Id str)) = str

-- methodType :: Id -> ClassName -> ClassTable -> Result Type
-- methodType mname cname ct = 
--     findClass cname ct     >>= \cdecl -> 
--     methodDecl mname cdecl >>= \mdecl -> 
--     return $ fargsToType (methodFormalArgs mdecl) cname 
--  --   case foundMethod of

-- test_prog = CProgram [CDecl (Id "teste") ClassObject [FDecl ClassObject (Id "a")] (KDecl (Id "teste") [Field ClassObject (Id "a")] [] [Assgnmt (Id "a") (Id "a")]) [], CDecl (Id "teste2") (ClassId $ Id "teste") [] (KDecl (Id "teste2") [] [] []) []] (NewExp (ClassId $ Id "teste") [])

-- test_prog2 = CProgram [CDecl (Id "A") ClassObject [] (KDecl (Id "A") [] [] []) [],CDecl (Id "B") ClassObject [] (KDecl (Id "B") [] [] []) [],CDecl (Id "Pair") ClassObject [FDecl (ClassId (Id "A")) (Id "fst"),FDecl (ClassId (Id "B")) (Id "snd")] (KDecl (Id "Pair") [Field (ClassId (Id "A")) (Id "fst"),Field (ClassId (Id "B")) (Id "snd")] [] [Assgnmt (Id "fst") (Id "fst"),Assgnmt (Id "snd") (Id "snd")]) [MDecl (ClassId (Id "Pair")) (Id "setfst") [FArg (ClassId (Id "A")) (Id "newfst")] (NewExp (ClassId (Id "Pair")) [ExpVar (Id "newfst"),NewExp (ClassId (Id "B")) []])]] (NewExp (ClassId (Id "Pair")) [NewExp (ClassId (Id "A")) [],NewExp (ClassId (Id "B")) []])

-- test_progCT = programCT test_prog
-- test_prog2CT = programCT test_prog2
-- a_class = findClass (ClassId $ Id "A") test_prog2CT
-- pair_class = findClass (ClassId $ Id "Pair") test_prog2CT
-- setfst_body = methodDecl (Id "setfst") pair_class

-- setfst_type = methodType (Id "setfst") (ClassId $ Id "Pair") test_prog2CT

