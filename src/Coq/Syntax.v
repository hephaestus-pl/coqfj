Require Import List.

Notation Id := nat.
Notation "'[' X ']'" := (list X) (at level 40).
(* We will use Notation to make automation easier*)

Inductive ClassName : Type :=
  | ClassObject : ClassName
  | ClassId : Id -> ClassName.
 
Inductive Var : Type :=
  | This : Var
  | VarId: Id -> Var.

Inductive Argument : Type :=
  | Arg : Id -> Argument.

Inductive Assignment : Type :=
  | Assgnmt : Id -> Id -> Assignment.

Inductive FormalArg : Type :=
  | FArg : ClassName -> Id -> FormalArg.

Inductive Constructor : Type :=
  | KDecl : Id -> [FormalArg] -> [Argument] -> [Assignment] -> Constructor.

Inductive FieldDecl : Type :=
  | FDecl : ClassName -> Id -> FieldDecl.

Inductive Exp : Type :=
  | ExpVar : Var -> Exp
  | ExpFieldAccess : Exp -> Id -> Exp
  | ExpMethodInvoc: Exp -> Id -> [Exp] -> Exp
  | ExpCast : ClassName -> Exp -> Exp
  | ExpNew : Id -> [Exp] -> Exp.

Inductive MethodDecl : Type :=
  | MDecl : ClassName -> Id -> [FormalArg] -> Id -> MethodDecl.

Inductive ClassDecl : Type :=
  | CDecl: Id -> ClassName -> [FieldDecl] -> Constructor -> [MethodDecl] -> ClassDecl.

Inductive Program : Type :=
  | CProgram : [ClassDecl] -> Exp -> Program.


