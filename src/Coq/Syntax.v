Require Import List.
Require Import SfLib.

Notation "'[' X ']'" := (list X) (at level 40).
(* We will use Notation to make automation easier
 * This will be the notation to be similar with haskell *)

Inductive ClassName :=
  | ClassObject : ClassName
  | ClassId : id -> ClassName.
 
Inductive Var :=
  | This : Var
  | VarId: id -> Var.

Inductive Argument :=
  | Arg : id -> Argument.

Inductive Assignment :=
  | Assgnmt : id -> id -> Assignment.

Inductive FormalArg :=
  | FArg : ClassName -> id -> FormalArg.

Inductive Constructor :=
  | KDecl : id -> [FormalArg] -> [Argument] -> [Assignment] -> Constructor.

Inductive FieldDecl :=
  | FDecl : ClassName -> id -> FieldDecl.

Inductive Exp : Set :=
  | ExpVar : Var -> Exp
  | ExpFieldAccess : Exp -> id -> Exp
  | ExpMethodInvoc: Exp -> id -> [Exp] -> Exp
  | ExpCast : ClassName -> Exp -> Exp
  | ExpNew : id -> [Exp] -> Exp.

Inductive MethodDecl :=
  | MDecl : ClassName -> id -> [FormalArg] -> id -> MethodDecl.

Inductive ClassDecl:=
  | CDecl: id -> ClassName -> [FieldDecl] -> Constructor -> [MethodDecl] -> ClassDecl.

Inductive Program :=
  | CProgram : [ClassDecl] -> Exp -> Program.

Parameter CT: @partial_map ClassDecl.
(*We will assume a global CT to make our definitions easier
 *To instance the CT use Hypothesis x: CT = ... *)

Definition extends (C D : ClassName) :=
  match C with
  | ClassObject => None
  | ClassId cname => find cname CT
  end.

  Print extends.


Reserved Notation "C '<:' D " (at level 40).

Inductive Subtype : ClassName -> ClassName -> Set :=
  | S_Refl: forall C: ClassName, C <: C
  | S_Trans: forall (C D E: ClassName), 
    C <: D -> 
    D <: E -> 
    C <: E
  | S_Decl: forall C D, 
    (exists decl, extends C D = Some decl) -> 
    C <: D
where "C '<:' D" := (Subtype C D).

Definition Bind := @partial_map Exp.


Lemma eq_var_dec : forall (v v': Var), {v = v'} + {v <> v'}.
Proof.
  intros. destruct v, v'; try eauto.
  Case "This VarId".
    right; intro.
    inversion H.
  Case "VarId This".
    right; intro; inversion H.
  Case "VarId VarId".
    destruct i, i0.
    destruct eq_nat_dec with (n:=n) (m:=n0).
    SCase "n = n0".
      left; rewrite e; reflexivity.
    SCase "n <> n0".
      right; intro H.
      inversion H. auto.
Defined.

Fixpoint subst (e: Exp) (v: Var) (v': Exp) :=
  match e with
  | ExpVar var => if eq_var_dec var v then v' else ExpVar var
  | ExpFieldAccess exp i => ExpFieldAccess (subst exp v v') i
  | ExpMethodInvoc exp i exps => 
      ExpMethodInvoc (subst exp v v') i (map (fun x => subst x v v') exps)
  | ExpCast cname exp => ExpCast cname (subst exp v v')
  | ExpNew cname exps => ExpNew cname (map (fun x => subst x v v') exps)
  end.

Notation " '[' v ':=' v' ']' e " := (subst e v v') (at level 40).  

Eval compute in ([ (VarId (Id 1)) := ExpFieldAccess (ExpVar This) (Id 2)] ExpVar (VarId (Id 1))).

