
module FJ.Dynamics.Environment where 

import FJ.TypeSystem.Types
import FJ.Syntax.Absfj_syntax

type Env = [(String, Exp)]

type Stack = [Env]

push :: Env -> Stack -> Stack
push env s = env : s

pop :: Stack -> (Env, Stack)
pop [] = ([],[]) 
pop (e:es) = (e, es)

top :: Stack -> Env
top (e:es) = e
