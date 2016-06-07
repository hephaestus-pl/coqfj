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

import FJ.Core.CommonTypes 

import FJ.TypeSystem.Types
import FJ.Syntax.Absfj_syntax

programCT :: Program -> ClassTable
programCT (CProgram ct _) = ct

programExp :: Program -> Exp
programExp (CProgram _ exp) = exp

className :: ClassDecl -> Id
className (CDecl id _ _ _ _) = id

findSuper :: ClassName -> ClassTable -> Result ClassName
findSuper cname ct = do
    cdecl <- find (ref cname) ct
    return (superClassOf cdecl) 

superClassOf :: ClassDecl -> ClassName
superClassOf (CDecl _ s _ _ _ ) = s

classFields :: ClassDecl -> [FieldDecl]
classFields (CDecl _ _ fdls _ _) = fdls

classMethods :: ClassDecl -> [MethodDecl]
classMethods (CDecl _ _ _ _ mdls) = mdls

fieldType :: FieldDecl -> ClassName
fieldType (FDecl cname _) = cname

methodFormalArgs :: MethodDecl -> [FormalArg]
methodFormalArgs (MDecl _ _ args _) = args

methodBody :: MethodDecl -> Exp
methodBody (MDecl _ _ _ body) = body

fargToVar :: FormalArg -> Var
fargToVar (FArg _ id)  = VarId id

fieldToVar :: FieldDecl -> Var
fieldToVar (FDecl _ id) = VarId id

fargsToType :: [FormalArg] -> ClassName -> Type
fargsToType [] cn = CType cn
fargsToType (FArg farType _ :xs) cd = CType farType :~>: fargsToType xs cd

methodDecl :: Id -> ClassDecl -> Result MethodDecl
methodDecl mname (CDecl _ _ _ _ mdecls) =
  case filter (\m -> (ref m) == mname) mdecls of
      [] -> raise $ "Couldn't find method with name " ++ show mname
      x:xs -> Ok x

returnType :: Type -> ExpType
returnType (CType t) = t
returnType (_ :~>: t) = returnType t

argTypes :: Type -> Type
argTypes tp@(CType t) = tp
argTypes (t1 :~>: CType t) = t1
argTypes (t1 :~>: t2) = argTypes t1 :~>: argTypes t2

typesToList :: Type -> [ClassName]
typesToList (CType t) = [t]
typesToList (t1 :~>: t2) = typesToList t1 ++ typesToList t2

fargType :: FormalArg -> ExpType
fargType (FArg t _) = t

fDeclType :: FieldDecl -> ExpType
fDeclType (FDecl t _) = t

argTypesL :: Type -> [ClassName]
argTypesL = typesToList . argTypes

mType :: Id -> ClassName -> ClassTable -> Result Type
mType mname cname ct = do
    cdecl <- find (ref cname) ct
    mdecl <- methodDecl mname cdecl
    return $ fargsToType (methodFormalArgs mdecl) cname 

fields :: Id -> ClassTable -> Result [FieldDecl]
fields (Id "Object") _ = return []
fields cname ct = do
    CDecl _ superName flds _ _<- find (cname) ct
    superFields <- fields (ref superName) ct
    return $ flds ++ superFields

instance Instanciable FieldDecl where
    typing (FDecl t _) = t
 
instance Instanciable FormalArg where
    typing (FArg t _) = t

instance Referable ClassDecl where 
    ref (CDecl id _ _ _ _) = id
    -- we override the find implementation to add the class object to the list of class declarations
    find key list = let objKons = KDecl (Id "Object") [] [] [] in
        case [x | x <- (CDecl (Id "Object") ClassObject [] objKons []):list, key == (ref x)] of
        []    -> raise $ "there is no such a key " ++ show key ++ " in the list."
        (x:_) -> return x  

instance Referable FieldDecl where 
    ref (FDecl _ id) = id

instance Referable MethodDecl where 
    ref (MDecl _ id _ _) = id

instance Referable ClassName where
    ref ClassObject = Id "Object"
    ref (ClassId id) = id

instance Referable Id where
    ref id = id

instance Referable FormalArg where
    ref (FArg _ id) = id

instance Referable Var where
    ref This = Id "this"
    ref (VarId id) = id

instance Referable TypeBind where
    ref (TypeBind (arg, _)) = ref arg

instance Referable Bind where
    ref (Bind (arg, _)) = ref arg

instance Referable Argument where
    ref (Arg id) = id


--   case foundMethod of

-- test_prog = CProgram [CDecl (Id "teste") ClassObject [FDecl ClassObject (Id "a")] (KDecl (Id "teste") [Field ClassObject (Id "a")] [] [Assgnmt (Id "a") (Id "a")]) [], CDecl (Id "teste2") (ClassId $ Id "teste") [] (KDecl (Id "teste2") [] [] []) []] (ExpNew (ClassId $ Id "teste") [])

-- test_prog2 = CProgram [CDecl (Id "A") ClassObject [] (KDecl (Id "A") [] [] []) [],CDecl (Id "B") ClassObject [] (KDecl (Id "B") [] [] []) [],CDecl (Id "Pair") ClassObject [FDecl (ClassId (Id "A")) (Id "fst"),FDecl (ClassId (Id "B")) (Id "snd")] (KDecl (Id "Pair") [Field (ClassId (Id "A")) (Id "fst"),Field (ClassId (Id "B")) (Id "snd")] [] [Assgnmt (Id "fst") (Id "fst"),Assgnmt (Id "snd") (Id "snd")]) [MDecl (ClassId (Id "Pair")) (Id "setfst") [FArg (ClassId (Id "A")) (Id "newfst")] (ExpNew (ClassId (Id "Pair")) [ExpVar (Id "newfst"),ExpNew (ClassId (Id "B")) []])]] (ExpNew (ClassId (Id "Pair")) [ExpNew (ClassId (Id "A")) [],ExpNew (ClassId (Id "B")) []])

-- test_progCT = programCT test_prog
-- test_prog2CT = programCT test_prog2
-- a_class = findClass (ClassId $ Id "A") test_prog2CT
-- pair_class = findClass (ClassId $ Id "Pair") test_prog2CT
-- setfst_body = methodDecl (Id "setfst") pair_class

-- setfst_type = methodType (Id "setfst") (ClassId $ Id "Pair") test_prog2CT

