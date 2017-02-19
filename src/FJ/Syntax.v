Require Import String.
Require Import FJ.Lists.
Require Import FJ.Base.
(* We will use Notation to make automation easier
 * This will be the notation to be similar with haskell *)
Notation "'[' X ']'" := (list X) (at level 40).

Definition ClassName := id.
Parameter Object: ClassName.

Definition Var := id.
Parameter this: Var.

Inductive Argument :=
  | Arg : id -> Argument.

Inductive FormalArg :=
  | FArg : ClassName -> id -> FormalArg.

Instance FargRef : Referable FormalArg :={
  ref farg := 
    match farg with 
   | FArg _ id => id end
}.

Definition fargType (f: FormalArg):ClassName := 
  match f with FArg t _ => t end.

Inductive FieldDecl :=
  | FDecl : ClassName -> id -> FieldDecl.

Instance FieldRef : Referable FieldDecl :={
  ref fdecl := 
    match fdecl with 
   | FDecl _ id => id end
}.

Definition fieldType (f: FieldDecl): ClassName := 
  match f with FDecl t _ => t end.

Inductive Exp : Type :=
  | ExpVar : Var -> Exp
  | ExpFieldAccess : Exp -> id -> Exp
  | ExpMethodInvoc : Exp -> id -> [Exp] -> Exp
  | ExpCast : ClassName -> Exp -> Exp
  | ExpNew : id -> [Exp] -> Exp.

Inductive Assignment :=
  | Assgnmt : Exp -> Exp -> Assignment.


Inductive Constructor :=
  | KDecl : id -> [FormalArg] -> [Argument] -> [Assignment] -> Constructor.


(* Arguments cannot have duplicate names *)
Inductive MethodDecl :=
  | MDecl : ClassName -> id -> forall (fargs: [FormalArg]), NoDup (this :: refs fargs) -> Exp -> MethodDecl.


Instance MDeclRef : Referable MethodDecl :={
  ref mdecl := 
    match mdecl with 
   | MDecl _ id _ _ _ => id end
}.

Inductive ClassDecl:=
  | CDecl: id -> ClassName -> 
    forall (fDecls:[FieldDecl]), NoDup (refs fDecls) -> Constructor -> 
    forall (mDecls:[MethodDecl]), NoDup (refs mDecls) -> ClassDecl.

Instance CDeclRef : Referable ClassDecl :={
  ref cdecl := 
    match cdecl with 
   | CDecl id _ _ _ _ _ _ => id end
}.

Inductive Program :=
  | CProgram : forall (cDecls: [ClassDecl]), NoDup (refs cDecls) -> Exp -> Program.

(* We assume a fixed ClassTable *)
Parameter CT: [ClassDecl].