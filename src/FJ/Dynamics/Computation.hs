module FJ.Dynamics.Computation where

import FJ.Syntax.Absfj_syntax 
import FJ.TypeSystem.Types

import FJ.Syntax.LookupFunctions

import Core.CommonTypes

computation :: Exp -> ClassTable -> Result Exp

computation obj@(NewExp name args) ct = return obj

computation (ExpFieldAccess exp field) ct = 
  computation exp ct >>= \(NewExp (ClassId (Id name)) args) -> 
  find name (map snd ct) >>= \(CDecl _ _ flds _ _) ->
  return (zip flds args) >>= \bind -> --refactor to let \/
  return (head [exp | ((FDecl _ f), exp) <- bind, f == field]) >>= \e -> 
  computation e ct 

computation (ExpMethodInvoc exp method args) ct =
  computation exp ct        >>= \obj@(NewExp cname@(ClassId name) _) -> 
  find (ref cname) (map snd ct)    >>= \(CDecl _ _ _ _ methods) ->
  find (ref method) methods >>= \(MDecl _ _ fargs body) ->
  return ((FArg cname (Id "this"), obj):(zip fargs args)) >>= \bind        ->
  computation (sub body bind) ct
  
sub :: Exp -> [(FormalArg, Exp)] -> Exp
sub = undefined 
