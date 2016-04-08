module FJ.TypeSystem.Lookup_functions where

import FJ.TypeSystem.Types
import FJ.Syntax.Absfj_syntax


get_ct :: Prog -> [CD]
get_ct (Program ct _) = ct 

transFD_Field :: [FD] -> [Field]
transFD_Field [] = []
transFD_Field ((FDecl t i):xs) = (Field t i):transFD_Field xs

field_lookup' :: [CD] -> [CD] -> Type -> [Field]
field_lookup' _ _ TypeObject = []
field_lookup' [] _ _ = []
field_lookup' ct@(CDecl id parent_class fields  _ _ : xs) consumed (TypeId id') = 
            if id == id' then (transFD_Field fields) ++ (field_lookup (ct++consumed) parent_class)
            else field_lookup' xs (x:consumed) (TypeId id') 
  where x = head ct

field_lookup :: [CD] -> Type -> [Field]
field_lookup ct ty = field_lookup' ct [] ty

test_prog = Program [CDecl (Id "teste") TypeObject [FDecl TypeObject (Id "a")] (KDecl (Id "teste") [Field TypeObject (Id "a")] [] [Assignment (Id "a") (Id "a")]) [], CDecl (Id "teste2") (TypeId $ Id "teste") [] (KDecl (Id "teste2") [] [] []) []] (NewExp (Id "teste") [])

test_progCT = get_ct test_prog

