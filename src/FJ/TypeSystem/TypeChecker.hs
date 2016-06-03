{-
Module      :  FJ.TypeSystem.Lookup_functions
Description :  Functions to retrieve information about types in a FJ program
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


module FJ.TypeSystem.TypeChecker where

import FJ.TypeSystem.Types
import FJ.Syntax.Absfj_syntax
import FJ.Syntax.LookupFunctions
import FJ.Core.CommonTypes
import Control.Monad


(<:) :: ClassName -> ClassName -> ClassTable-> Result ()
(c <: ClassObject) _ = return ()
(c <: d) ct = do
    superC <- findSuper c ct 
    if ref c == ref d then return () 
    else if superC == ClassObject 
        then raise $ show d ++ " Not super of " ++ show c -- The only way a program get suck is if it cannot perform a downcast
    else do
        superC <: d $ ct

--the comments is because there is no such thing in the paper
--we should check those by the end
expType :: Exp -> Gamma -> ClassTable -> Result ExpType
expType (ExpVar x) gamma ct = do
    TypeBind (_, t) <- find (ref x) gamma
    return t

expType (ExpFieldAccess exp id) gamma ct = do
    c0 <- expType exp gamma ct
    CDecl _ _ fields _ _ <- find (ref c0) ct
    FDecl cname _ <- find (ref id) fields
    return cname 

expType (ExpNew cname exps) gamma ct = do
    CDecl _ _ fields _ _ <- find (ref cname) ct
    let cFields = map (fieldType) fields
    expsTypes <- mapM (\e -> expType e gamma ct) exps
    zipWithM_ (\c d -> (c <: d) ct) expsTypes cFields -- checks whether each variable passed as argument has the correct type
    return $ ClassId cname

expType (ExpMethodInvoc exp id exps) gamma ct = do
    c0 <- expType exp gamma ct
    mtype <- mType id c0 ct
    expsTypes <- mapM (\e -> expType e gamma ct) exps
    let argtypes = typesToList $ argTypes mtype
    zipWithM_ (\c d -> (c <: d) ct) expsTypes argtypes
    return $ returnType mtype

expType (ExpCast cname exp) gamma ct = do
    d <- expType exp gamma ct
    CDecl _ c _ _ _ <- find (ref cname) ct
    _ <- (c <: d) ct
    return $ cname

mOkIn :: MethodDecl -> ClassName -> ClassTable -> Result ()
((MDecl c0 m fargs exp) `mOkIn` c) ct = do
    let gamma = map TypeBind $ (This, c) : zip (map fargToVar fargs) (map fargType fargs)
    e0 <- expType exp gamma ct
    _ <- (e0 <: c0) ct
    CDecl _ d _  _ _ <- find (ref c0) ct
    case mType m d ct of
        Ok t2 -> do 
            t1 <- (mType m c ct)
            sameTypes t1 t2
        Ex _ -> return ()

classOk :: ClassDecl -> ClassTable -> Result ()
classOk (CDecl cId d fdecls kons mdecls) ct =  
    case kons of 
    KDecl kId fargs superArgs assnmts -> do
    compareClassId kId cId
    dfields <- fields (ref d) ct
    let superFargsIds = map ref superArgs
    let superFargs = filter (\x -> (ref x) `elem` superFargsIds) fargs
    _ <- compareFargFields superFargs dfields
    mapM_ (\m -> (m `mOkIn` (ClassId cId)) ct) mdecls

compareFargFields :: [FormalArg] -> [FieldDecl] -> Result ()
compareFargFields [] [] = return () 
compareFargFields [] _ = raise "SuperClass with not that many fields"
compareFargFields _ [] = raise "Too Little Fields provided to super"
compareFargFields ((FArg farType farId):fargs) fields = do
    (FDecl fType fId) <- find farId fields
    if farType == fType 
    then compareFargFields fargs fields
    else raise $ "Field " ++ show fId ++ " with tpes"


sameTypes :: Type -> Type -> Result ()
sameTypes t1 t2 = 
    if t1 == t2 then return ()
    else raise $ "Method override with different types"

    
compareClassId cId kId = 
    if cId == kId 
    then return ()
    else raise $ "Constructor Name: " ++ show(kId) ++ "does not match class Name: " ++ show (kId) 


