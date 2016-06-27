Require Import List.

Notation Id := nat.
Notation "'[' X ']'" := (list X) (at level 40).
(* We will use Notation to make automation easier
 * This will be the notation to be similar with haskell *)

Inductive ClassName :=
  | ClassObject : ClassName
  | ClassId : Id -> ClassName.
 
Inductive Var :=
  | This : Var
  | VarId: Id -> Var.

Inductive Argument :=
  | Arg : Id -> Argument.

Inductive Assignment :=
  | Assgnmt : Id -> Id -> Assignment.

Inductive FormalArg :=
  | FArg : ClassName -> Id -> FormalArg.

Inductive Constructor :=
  | KDecl : Id -> [FormalArg] -> [Argument] -> [Assignment] -> Constructor.

Inductive FieldDecl :=
  | FDecl : ClassName -> Id -> FieldDecl.

Inductive Exp :=
  | ExpVar : Var -> Exp
  | ExpFieldAccess : Exp -> Id -> Exp
  | ExpMethodInvoc: Exp -> Id -> [Exp] -> Exp
  | ExpCast : ClassName -> Exp -> Exp
  | ExpNew : Id -> [Exp] -> Exp.

Inductive MethodDecl :=
  | MDecl : ClassName -> Id -> [FormalArg] -> Id -> MethodDecl.

Inductive ClassDecl :=
  | CDecl: Id -> ClassName -> [FieldDecl] -> Constructor -> [MethodDecl] -> ClassDecl.

Inductive Program :=
  | CProgram : [ClassDecl] -> Exp -> Program.

Parameter CT: [ClassDecl].
(*We will assume a global CT to make our definitions easier
 *To instance the CT use Hypothesis x: CT = ... *)

Definition extends (C D : ClassName) : Prop :=
  match C with
  | ClassObject => False
  | ClassId cname =>
    exists fdecls kons mdecl, List.In (CDecl cname D fdecls kons mdecl) CT
  end.


Reserved Notation "C '<:' D " (at level 40).

Inductive Subtype : ClassName -> ClassName -> Set :=
  | S_Refl: forall C:ClassName, C <: C
  | S_Trans: forall (C D E: ClassName), 
    C <: D -> D <: E -> C <: E
  | S_Decl: forall C D, 
    extends C D -> C <: D
where "C '<:' D" := (Subtype C D).

