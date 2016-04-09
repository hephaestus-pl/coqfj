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
import FJ.Lookup_functions


