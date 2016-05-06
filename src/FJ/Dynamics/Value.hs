module FJ.Dynamics.Value where

import FJ.Dynamics.Environment

import Core.CommonTypes

import FJ.Syntax.Absfj_syntax
import FJ.TypeSystem.Types

data Value  = ClassInstance ClassName Env
              deriving(Eq, Show, Ord)


instance Referable Value where
  ref (ClassInstance (ClassId (Id s)) _) = s

state :: Value -> Env
state (ClassInstance _ env) = env

createInstance :: String -> Env -> Value
createInstance cname state = ClassInstance (ClassId (Id cname)) state
-- newObj = ClassInstance (ClassId (Id "obj")) []




