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

eval :: ClassTable -> Exp -> Result Value
eval ct (NewExp cname args) = do 
    cdecl <- findClass cname ct
    let fields = classFields cdecl in do
        l <- mZip (map fieldId fields) args
        return $ ClassInstance cname [(x,y) | (x,y) <- l]
    
