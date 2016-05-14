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
import Core.CommonTypes


isSubType :: ClassDecl -> ClassName -> Result ()
c `isSubType` ClassObject = return ()
c `isSubType` d = 
    let superC = superClassOf c in
    if ref c == ref d then return () -- reflexive
    else if superC == ClassObject 
        then raise $ show d ++ "Not super of" ++ show c
    else superC `isSubType` d 

expType :: Exp -> Gamma -> ClassTable -> Result ExpType
expType (ExpVar x) gamma _ = do
    TypeBind (_, t) <- find (ref x) gamma
    return t
expType (ExpFieldAccess exp id) gamma ct = do
    c0 <- expType exp gamma ct
    CDecl _ _ fields _ _ <- find (ref c0) (map snd ct)
    FDecl cname _ <- find (ref id) fields
    return cname 
expType (ExpNew cname exps) gamma ct = do
    CDecl _ _ fields _ _ <- find (ref cname) (map snd ct)
    cFields <- mapM (fieldType) fields
    expsTypes <- mapM (\e -> expType e gamma ct) exps
    expsDecls <- mapM (\e -> find (ref e) (map snd ct)) expsTypes
    --let ok = [(c,d) | c <- expsDecls , d <- cFields , c `isSubType` d] 
    let ok = zipWith (\c d -> c `isSubType` d) expsDecls cFields 
    return cname
    --_ <- zipWith (\c d -> c `isSubType` d) expsDecls cFields



