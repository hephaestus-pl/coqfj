Require Import FJ.Lists.
Require Import FJ.Base.
Require Import FJ.Syntax.
Require Import Decidable Relations.

Reserved Notation "C '<:' D 'at' CT" (at level 40).
Inductive Subtype : [ClassDecl] -> id -> ClassName -> Prop :=
  | S_Refl: forall C: ClassName, C <: C at CT 
  | S_Trans: forall (C D E: ClassName), 
    C <: D at CT-> 
    D <: E at CT-> 
    C <: E at CT
  | S_Decl: forall C D fs noDupfs K mds noDupMds,
    find C CT = Some (CDecl C D fs noDupfs K mds noDupMds ) ->
    C <: D at CT
where "C '<:' D 'at' CT" := (Subtype CT C D).
Hint Constructors Subtype.

Tactic Notation "subtype_cases" tactic(first) ident(c) :=
  first;
  [ Case_aux c "S_Refl" | Case_aux c "S_Trans" 
  | Case_aux c "S_Decl"].
Print antisymmetric.

Inductive ClassTable: [ClassDecl] -> Prop :=
  | CT_nil : ClassTable nil
  | CT_cons : forall CT CD C D fDecls noDupfDecls K mDecls noDupmDecls D' fDecls' noDupfDecls' K' mDecls' noDupmDecls', 
    ClassTable CT ->
    NoDup (refs (CD :: CT)) -> 
    antisymmetric _ (Subtype (CD :: CT)) ->
    find Object CT = None ->
    C <: Object at CT ->
    CD = CDecl C D fDecls noDupfDecls K mDecls noDupmDecls ->
    find D CT = Some (CDecl D D' fDecls' noDupfDecls' K' mDecls' noDupmDecls') ->
    ClassTable (CD :: CT).

Fixpoint find_CT (C: ClassName) (CT : ClassTable): Option ClassDecl :=
  match CT with
  | CT_nil => None
  | CT_Cons CT' CD C' _ _ _ _ _ _ _ _ _ _ _ _ => 
    if beq_id C C' then Some CD
    else find_CT CT'
  end.