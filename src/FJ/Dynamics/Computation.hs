module FJ.Dynamics.Computation where

import FJ.Syntax.Absfj_syntax 
import FJ.TypeSystem.Types
import FJ.TypeSystem.TypeChecker
import FJ.Syntax.LookupFunctions
import Core.CommonTypes

computation :: Exp -> ClassTable -> Result Exp

computation obj@(ExpNew name args) ct = return obj

computation (ExpFieldAccess exp field) ct = 
  computation exp ct >>= \(ExpNew (Id name) args) -> 
  find name (map snd ct) >>= \(CDecl _ _ flds _ _) ->
  let vars = map fieldToVar flds in 
  let bind = map bindTuple $ zip vars args in
  find (ref field) bind >>= \(Bind (_, e)) -> 
  --(head [exp | ((FDecl _ f), exp) <- bind, f == field]) >>= \e ->
  computation e ct 

computation (ExpMethodInvoc exp method args) ct =
  computation exp ct        >>= \obj@(ExpNew cname _) -> 
  find (ref cname) (map snd ct)    >>= \(CDecl _ _ _ _ methods) ->
  find (ref method) methods >>= \(MDecl _ _ fargs body) ->
  let bind = (Bind (This, obj):(map bindTuple $ zip (map fargToVar fargs) args)) in
  subst body bind >>= \substExp ->
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
    substExps <- mapM (\e -> (subst e bind)) exps
    return $ ExpMethodInvoc substExp id substExps
subst (ExpCast cname exp) bind = do
    substExp <- subst exp bind
    return $ ExpCast cname substExp
subst (ExpNew cname exps) bind = do
    substExps <- mapM (\e -> (subst e bind)) exps
    return $ ExpNew cname substExps

    
