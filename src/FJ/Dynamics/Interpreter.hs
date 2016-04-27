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

-- a funcao classFields recebe uma ClassDecl, nao uma ClassId
-- temos primeiro que achar a declaracao da classe na CT
-- esta mesma funcao retorna uma lista de FieldDecl
-- precisamos pegar apenas o Id para que o Object que estamos
-- retornando seja um Value valido
eval :: ClassTable -> Exp -> Value
eval ct (NewExp cname args) = 
    case findClass cname ct of
        Nothing -> error "no class found with that name"
        Just cdecl -> ClassInstance cname [(x,y) | (x,y) <- l]
            where fields = classFields cdecl 
                  l = zip (map fieldId fields) args
    
