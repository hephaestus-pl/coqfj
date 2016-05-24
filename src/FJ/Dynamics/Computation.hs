module FJ.Dynamics.Computation where

import FJ.Syntax.Absfj_syntax 
import FJ.TypeSystem.Types
import FJ.TypeSystem.TypeChecker
import FJ.Syntax.LookupFunctions
import FJ.Core.CommonTypes

computation :: Exp -> ClassTable -> Result Exp

computation obj@(ExpNew _ _) ct = do
    _ <- expType obj [] ct
    return obj

computation obj@(ExpCast cname exp) ct = do
    _ <- expType obj [] ct
    e <- computation obj ct
    return $ ExpCast cname e
--cast binds less tightly

computation t@(ExpFieldAccess exp field) ct = do
    (ExpNew name args) <- computation exp ct
    (CDecl _ c flds _ _) <- find name ct
    let vars = map fieldToVar flds 
    let bind = map Bind $ zip vars args
    argTypes <- mapM (\arg -> expType arg [] ct) args
    let gamma = map TypeBind $ zip vars argTypes
    _ <- expType t gamma ct
    (Bind (_, e)) <- find (ref field) bind
    computation e ct 

computation t@(ExpMethodInvoc exp method args) ct = do
    obj@(ExpNew cname flds) <- computation exp ct 
    (CDecl _ c  _ _ methods) <- find (ref cname) ct
    (MDecl _ _ fargs body) <- find (ref method) methods 
    let vars = map fargToVar fargs
    let bind = (Bind (This, obj):(map Bind $ zip vars args))
    argTypes <- mapM (\arg -> expType arg [] ct) args 
    let gamma = (TypeBind (This, c):(map TypeBind $ zip vars argTypes))
    _ <- expType t gamma ct
    substExp <- subst body bind 
    computation substExp ct
  
subst :: Exp -> [Bind] -> Result Exp
subst (ExpVar var) bind = do
    Bind (_, e) <- find (ref var) bind 
    return e
subst (ExpFieldAccess exp id) bind = do
    substExp <- subst exp bind
    return $ ExpFieldAccess substExp id
subst (ExpMethodInvoc exp id exps) bind = do
    substExp <- subst exp bind
    substExps <- mapM (\e -> (subst e bind)) exps -- also substitute in the expression list
    return $ ExpMethodInvoc substExp id substExps
subst (ExpCast cname exp) bind = do
    substExp <- subst exp bind
    return $ ExpCast cname substExp
subst (ExpNew cname exps) bind = do
    substExps <- mapM (\e -> (subst e bind)) exps
    return $ ExpNew cname substExps

    
