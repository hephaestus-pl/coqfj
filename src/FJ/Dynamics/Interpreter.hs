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

import FJ.Dynamics.Value
import FJ.Dynamics.Environment

import FJ.Syntax.Absfj_syntax
import FJ.TypeSystem.Lookup_functions
import FJ.TypeSystem.Types


mZip :: [a] -> [b] -> Result [(a, b)]
mZip x1 x2 = if length x1 == length x2
                        then Ok $ zip x1 x2 
                        else raise "lists have different lengths, abort!"

findExp :: Env -> Id -> Result Exp
findExp [] id = raise $ (show id) ++ " not found"
findExp ((a,b):xs) id = if a == id 
                then Ok b
                else findExp xs id

eval :: Exp -> ClassTable -> Stack -> Result Value
eval (NewExp cname args) ct stack = do
  fields <- fmap classFields $ findClass cname ct
  env    <- mZip (map fieldId fields) args  
  return $ ClassInstance cname env

eval (ExpFieldAccess exp id) ct stack = do 
  value <- eval exp ct stack
  e <- findExp (state value) id
  eval e ct stack

eval (ExpMethodInvoc exp id args) ct stack = do
  value <- eval exp ct stack
  cdecl <- findClass (vName value) ct
  let (mfargs, mbody) = mbodyof id cdecl
  args <- mZip mfargs args
  let stack' = newBind exp [(n, e) | (FArg c n, e) <- args] stack
  eval mbody ct stack' 


newBind :: Exp -> Env -> Stack -> Stack 
newBind obj args stack = 
 let newEnv = (Id "this", obj) : args
 in push newEnv stack 
                 
