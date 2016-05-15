module FJ.Dynamics.Computation where

import FJ.Syntax.Absfj_syntax 
import FJ.TypeSystem.Types
import FJ.TypeSystem.TypeChecker
import FJ.Syntax.LookupFunctions
import FJ.Core.CommonTypes

computation :: Exp -> ClassTable -> Result Exp

computation obj@(ExpNew name args) ct = return obj

computation (ExpFieldAccess exp field) ct = do
  (ExpNew name args) <- computation exp ct
  (CDecl _ _ flds _ _) <- find name ct
  let vars = map fieldToVar flds 
  let bind = map bindTuple $ zip vars args
  (Bind (_, e)) <- find (ref field) bind
  --(head [exp | ((FDecl _ f), exp) <- bind, f == field]) >>= \e ->
  computation e ct 

computation (ExpMethodInvoc exp method args) ct = do
    obj@(ExpNew cname _) <- computation exp ct 
    (CDecl _ _ _ _ methods) <- find (ref cname) ct
    (MDecl _ _ fargs body) <- find (ref method) methods 
    let bind = (Bind (This, obj):(map bindTuple $ zip (map fargToVar fargs) args))
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

    
