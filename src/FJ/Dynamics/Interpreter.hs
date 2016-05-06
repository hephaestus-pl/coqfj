{-
Module      :  FJ.Dynamics.Interpreter
Description :  Interpreter Class
Copyright   :  Daniela & Pedro
License     :  <license>

Maintainer  :  abreu223@hotmail.com & daniaangelos@gmail.com
Stability   :  experimental 
Portability :  portable

<module description starting at first column>
-}

module FJ.Dynamics.Interpreter where

import Control.Applicative 
import Core.CommonTypes

import FJ.Dynamics.Value
import FJ.Dynamics.Environment

import FJ.Syntax.Absfj_syntax
import FJ.Syntax.LookupFunctions
import FJ.TypeSystem.Types


mZip :: [a] -> [b] -> Result [(a, b)]
mZip x1 x2 =
  if length x1 == length x2
   then Ok $ zip x1 x2
   else raise "lists have different lengths, abort!"

findExp :: Env -> String -> Result Exp
findExp [] id = raise $ (show id) ++ " not found"
findExp ((a,b):xs) id = if a == id 
                then Ok b
                else findExp xs id

eval :: Exp -> ClassTable -> Stack -> Result Value

-- eval *new* expression
eval (NewExp (ClassId (Id cname)) args) ct stack =
  find cname (map snd ct) >>= \c ->
  Ok (classFields c)      >>= \fs ->  
  mZip (map ref fs) args  >>= \s ->
  return $ createInstance cname s
 
-- do
--   fields <- fmap classFields $ find cname (map snd ct) 
--   env    <- mZip (map ref fields) args  
--   return $ createInstance cname env

-- eval *field access* expression
eval (ExpFieldAccess exp (Id f)) ct stack =
   eval exp ct stack     >>= \obj -> 
   findExp (state obj) f >>= \e   -> 
   eval e ct stack

--
-- eval *method invocation* expression
--
eval (ExpMethodInvoc exp (Id methodName) args) ct stack = do
  obj <- eval exp ct stack
  classDec <- find (ref obj) (map snd ct)
  methodDec <- find methodName (classMethods classDec)
  bind <- newBind exp methodDec args stack
  eval (methodBody methodDec) ct bind



newBind :: Exp -> MethodDecl -> [Exp] -> Stack -> Result Stack 
newBind obj method args stack = undefined
 -- let newEnv = (Id "this", obj) : args
 -- in push newEnv stack 
                 
