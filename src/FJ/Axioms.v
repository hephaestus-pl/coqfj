Require Import Decidable Relations.
Require Import Base.
Require Import FJ.Syntax.
Require Import FJ.Semantics.

(* Axioms for ClassTable sanity *)
Axiom dec_subtype: forall C D,
  decidable (Subtype C D).

Axiom antisym_subtype:
  antisymmetric _ Subtype.

Axiom ClassesOK: forall C D Fs noDupfs K Ms noDupMds, 
  find C CT = Some (CDecl C D Fs noDupfs K Ms noDupMds) ->
  CType_OK (CDecl C D Fs noDupfs K Ms noDupMds).

Axiom obj_notin_dom: find Object CT = None.
Hint Rewrite obj_notin_dom.

Axiom superClass_in_dom: forall C D Fs noDupfs K Ms noDupMds,
  find C CT = Some (CDecl C D Fs noDupfs K Ms noDupMds) ->
  D <> Object ->
  exists D0 Fs0 noDupfs0 K0 Ms0 noDupMds0, find D CT = Some (CDecl D D0 Fs0 noDupfs0 K0 Ms0 noDupMds0).