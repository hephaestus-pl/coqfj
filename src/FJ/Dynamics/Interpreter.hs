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

import FJ.Syntax.Absfj_syntax
import FJ.TypeSystem.Lookup_functions
import FJ.TypeSystem.Types
import FJ.Dynamics.Value

mZip :: [a] -> [b] -> Result [(a, b)]
mZip x1 x2 = if length x1 == length x2
                        then Ok $ zip x1 x2 
                        else raise "lists have different lengths, abort!"

--maybe its a good idea to make it get the Instance itself as argument
fieldValue :: FieldBindings -> Id -> Result Value
fieldValue [] id = raise $ (show id) ++ " not found"
fieldValue ((a,b):xs) id = if a == id 
                then Ok b
                else fieldValue xs id

popVar :: Env -> Var -> Result Value
popVar [] var = raise $ (show var) ++ " not found in env"
popVar ((v, val): xs) var = if v == var
                            then Ok val
                            else popVar xs var


evalRec :: ClassTable -> Env -> Exp -> Result Value
evalRec  _ env (ExpVar var) = popVar env var
evalRec ct env (NewExp cname args) = do 
    cdecl <- findClass cname ct
    let fields = classFields cdecl in do
    evalArgs <- mapM (evalRec ct env) args
    l <- mZip (map fieldId fields) evalArgs
    return $ ClassInstance cname l
evalRec ct env (ExpFieldAccess exp id) = do
    value <- evalRec  ct env exp
    let newEnv = ((This, value):env) in do
    e <- fieldValue (fieldBindings value) id
    return e
    --evalRec ct newEnv e
evalRec ct env (ExpMethodInvoc exp id args) = do
    value <- evalRec ct env exp
    cdecl <- findClass (vName value) ct
    let (mfargs, mbody) = mbodyof id cdecl in do
    let argsEnv = ((This, value):env) in do
    evalArgs <- mapM (evalRec ct argsEnv) args
    l <- mZip (map fargToVar mfargs) evalArgs
    let newEnv = l ++ argsEnv in do
    evalRec ct newEnv mbody -- we need to add the fargs to the env
    

eval ct env = evalRec ct [] env
