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
    if ref c == ref d then return () -- reflexive
    else if superC == ClassObject 
        then raise $ show d ++ " Not super of " ++ show c
    else do
        superC <: d $ ct

--the comments is because there is no such thing in the paper
--we should check those by the end
expType :: Exp -> Gamma -> ClassTable -> Result ExpType
expType (ExpVar x) gamma ct = do
    TypeBind (_, t) <- find (ref x) gamma
    -- find (ref t) ct -- checks wheter the variable in the binding is well defined at the ct
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
    -- mapM_ (\e -> find (ref e) ct) expsTypes-- checks whether each variable passed as argument has a type defined in the CT 
    -- mapM_ (\d -> find (ref d) ct) cFields    -- checks whether each variable passed as argument has a type defined in the CT 
    zipWithM_ (\c d -> (c <: d) ct) expsTypes cFields -- checks whether each variable passed as argument has the correct type
    return (ClassId cname)



