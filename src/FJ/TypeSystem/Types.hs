module FJ.TypeSystem.Types where

import FJ.Syntax.Absfj_syntax

data Type =
  | BType CTEntry
  | FType Type BType 

