module FJ.Syntax.Absfj_syntax where

-- Haskell module generated by the BNF converter


newtype Id = Id String deriving (Eq,Ord,Show)
data Program =
   CProgram [ClassDecl] Exp
  deriving (Eq,Ord,Show)

data ClassDecl =
   CDecl Id ClassName [FieldDecl] Constructor [MethodDecl]
  deriving (Eq,Ord,Show)

data FieldDecl =
   FDecl ClassName Id
  deriving (Eq,Ord,Show)

data Constructor =
   KDecl Id [FieldParam] [Argument] [Assignment]
  deriving (Eq,Ord,Show)

data FieldParam =
   Field ClassName Id
  deriving (Eq,Ord,Show)

data FormalArg =
   FArg ClassName Id
  deriving (Eq,Ord,Show)

data Argument =
   Arg Id
  deriving (Eq,Ord,Show)

data Assignment =
   Assgnmt Id Id
  deriving (Eq,Ord,Show)

data MethodDecl =
   MDecl ClassName Id [FormalArg] Exp
  deriving (Eq,Ord,Show)

data Exp =
   ExpVar Id
 | ExpFieldAccess Access Id
 | ExpMethodInvoc Access Id [Exp]
 | CastExp ClassName Exp
 | NewExp Id [Exp]
  deriving (Eq,Ord,Show)

data Access =
   ThisAccess
 | ExpAccess Exp
  deriving (Eq,Ord,Show)

data ClassName =
   ClassObject
 | ClassId Id
  deriving (Eq,Ord,Show)

