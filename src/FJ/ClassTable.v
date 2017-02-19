Require Import Decidable Relations.
Require Import Base.
Require Import FJ.Syntax.
Require Import FJ.Semantics.

(* We assume a fixed CT *)
Parameter CT: [ClassDecl].

Reserved Notation "C '<:' D " (at level 40).
Inductive Subtype : id -> ClassName -> Prop :=
  | S_Refl: forall C: ClassName, C <: C
  | S_Trans: forall (C D E: ClassName), 
    C <: D -> 
    D <: E -> 
    C <: E
  | S_Decl: forall C D fs noDupfs K mds noDupMds,
    find C CT = Some (CDecl C D fs noDupfs K mds noDupMds ) ->
    C <: D
where "C '<:' D" := (Subtype C D).
Hint Constructors Subtype.

Tactic Notation "subtype_cases" tactic(first) ident(c) :=
  first;
  [ Case_aux c "S_Refl" | Case_aux c "S_Trans" 
  | Case_aux c "S_Decl"].

(* Hypothesis for ClassTable sanity *)
Module CTSanity.
Hypothesis dec_subtype: forall C D,
  decidable (Subtype C D).

Hypothesis antisym_subtype:
  antisymmetric _ Subtype.

Hypothesis ClassesOK: forall C D Fs noDupfs K Ms noDupMds, 
  find C CT = Some (CDecl C D Fs noDupfs K Ms noDupMds) ->
  CType_OK (CDecl C D Fs noDupfs K Ms noDupMds).
Hint Resolve ClassesOK.

Hypothesis obj_notin_dom: find Object CT = None.
Hint Rewrite obj_notin_dom.

Hypothesis superClass_in_dom: forall C D Fs noDupfs K Ms noDupMds,
  find C CT = Some (CDecl C D Fs noDupfs K Ms noDupMds) ->
  D <> Object ->
  exists D0 Fs0 noDupfs0 K0 Ms0 noDupMds0, find D CT = Some (CDecl D D0 Fs0 noDupfs0 K0 Ms0 noDupMds0).

End CTSanity.