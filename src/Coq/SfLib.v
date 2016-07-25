(** * SfLib: Software Foundations Library *)

(* $Date: 2013-01-16 22:29:57 -0500 (Wed, 16 Jan 2013) $ *)

(** Here we collect together several useful definitions and theorems
    from Basics.v, List.v, Poly.v, Ind.v, and Logic.v that are not
    already in the Coq standard library.  From now on we can [Import]
    or [Export] this file, instead of cluttering our environment with
    all the examples and false starts in those files. *)

(** * From the Coq Standard Library *)

Require Omega.   (* needed for using the [omega] tactic *)
Require Export Bool.
Require Export List.
Require Export Arith.
Require Export Arith.EqNat.  (* Contains [beq_nat], among other things *)
Import ListNotations.
Require Import NPeano.

(* Identifiers and polymorphic partial maps. *)
Inductive id : Type := 
  Id : nat -> id.

Definition beq_id id1 id2 :=
  match (id1, id2) with
    (Id n1, Id n2) => beq_nat n1 n2
  end.

Theorem beq_id_refl : forall i,
  true = beq_id i i.
Proof.
  intros. destruct i.
  apply beq_nat_refl.  Qed.

Theorem beq_id_eq : forall i1 i2,
  true = beq_id i1 i2 -> i1 = i2.
Proof.
  intros i1 i2 H.
  destruct i1. destruct i2.
  apply beq_nat_eq in H. subst.
  reflexivity.  Qed.

Theorem beq_id_false_not_eq : forall i1 i2,
  beq_id i1 i2 = false -> i1 <> i2.
Proof.
  intros i1 i2 H.
  destruct i1. destruct i2.
  apply beq_nat_false in H.
  intros C. apply H. inversion C. reflexivity.  Qed.

Theorem not_eq_beq_id_false : forall i1 i2,
  i1 <> i2 -> beq_id i1 i2 = false.
Proof.
  intros i1 i2 H.
  destruct i1. destruct i2.
  assert (n <> n0).
    intros C. subst. apply H. reflexivity.
  apply beq_nat_false_iff. assumption.  Qed.

Theorem beq_id_sym: forall i1 i2,
  beq_id i1 i2 = beq_id i2 i1.
Proof.
  intros i1 i2. destruct i1. destruct i2. apply NPeano.Nat.eqb_sym. Qed.

Theorem eq_id_dec: forall (i1 i2: id),
  {i1 = i2} + {i1 <> i2}.
Proof.
  intros.
  destruct i1, i2.
  destruct eq_nat_dec with n n0.
  left; auto.
  right; intro.
  apply n1; inversion H; auto.
Qed.


Inductive partial_map {A : Set} := 
  | empty : partial_map
  | record : id -> A -> partial_map -> partial_map .

Definition update {A: Set} (d : partial_map)
                  (key : id) (value : A) : partial_map :=
           record key value d.

Section Ref.
Class Referable (a: Set) :={
  ref : a -> id;
  find: id -> list a -> option a := 
    let fix f (key: id) (l: list a) :=
    match l with
      | [] => None
      | (x :: xs) => if beq_id key (ref x) 
                      then Some x
                      else f key xs
    end in f
}.

Inductive findi {A: Set} {R: @Referable A} : id -> list A -> A -> Prop:=
  | find_head : forall x xs, findi (ref x) (x :: xs) (x)
  | find_step : forall k1 k2 l x, 
    k1 <> ref k2 -> findi k1 l x -> findi k1 (k2 :: l) x.



Lemma find_deterministic: forall (A: Set) (R: @Referable A) d (k1: id) (x1 x2: option A),
  find k1 d = x1 ->
  find k1 d = x2 ->
  x1 = x2.
Proof with eauto.
  intros.
  destruct x1, x2; 
  destruct (@find A) in *; auto with rewrite.
  rewrite <- H; auto.
  inversion H.
  inversion H0.
  inversion H.
Qed.

Lemma findi_diff: forall (A: Set) (R: @Referable A) k a v l,
  k = ref a -> a <> v -> ~findi k (a :: l) v.
Proof.
  intros.
  intro.
  inversion H1; subst.
  apply H0; auto.
  apply H5; auto.
Qed.

Lemma find_iff_findi: forall (A: Set) (R: @Referable A) d (k1: id) (x1: A),
  find k1 d = Some x1 <-> findi k1 d x1.
Proof.
  intros. split.
  intro H. 
  induction d.
    inversion H.

    destruct eq_id_dec with (i1:= k1) (i2:= ref a).
    rewrite e in H; simpl in H. rewrite <- beq_id_refl in H.
    inversion H.
    rewrite e. rewrite H1.
    constructor.
    
    unfold find in H.
    rewrite not_eq_beq_id_false in H; auto.
    fold (@find A) in H.
    constructor.
    auto.
    apply IHd; auto.

  intro.
  induction H.
    simpl. 
    rewrite <- beq_id_refl. auto.
    unfold find.
    rewrite not_eq_beq_id_false; auto.
Qed.

Lemma find_iff_findi': forall (A: Set) (R: @Referable A) d (k1: id) (x: A),
  find k1 d = None <-> ~ findi k1 d x.
Proof.
  intros.
  split.
  intro.
  intro.
  induction H0.
  simpl in H.
Admitted.


Lemma find_dec : forall (A: Set) (R: Referable A) k1 d (v: A),
  (forall (a1 a2: A), {a1 = a2} + {a1 <> a2}) ->
  {find k1 d = Some v} + {find k1 d = None}.
Proof.
  intros.
  induction d.
  right; auto.
  
  destruct IHd.
Admitted.

End Ref.

(** * From Basics.v *)

Definition admit {T: Type} : T.  Admitted.

Require String. Open Scope string_scope.

Ltac move_to_top x :=
  match reverse goal with
  | H : _ |- _ => try move x after H
  end.

Tactic Notation "assert_eq" ident(x) constr(v) :=
  let H := fresh in
  assert (x = v) as H by reflexivity;
  clear H.

Tactic Notation "Case_aux" ident(x) constr(name) :=
  first [
    set (x := name); move_to_top x
  | assert_eq x name; move_to_top x
  | fail 1 "because we are working on a different case" ].

Tactic Notation "Case" constr(name) := Case_aux Case name.
Tactic Notation "SCase" constr(name) := Case_aux SCase name.
Tactic Notation "SSCase" constr(name) := Case_aux SSCase name.
Tactic Notation "SSSCase" constr(name) := Case_aux SSSCase name.
Tactic Notation "SSSSCase" constr(name) := Case_aux SSSSCase name.
Tactic Notation "SSSSSCase" constr(name) := Case_aux SSSSSCase name.
Tactic Notation "SSSSSSCase" constr(name) := Case_aux SSSSSSCase name.
Tactic Notation "SSSSSSSCase" constr(name) := Case_aux SSSSSSSCase name.


(** * Some useful tactics *)

Tactic Notation "solve_by_inversion_step" tactic(t) :=  
match goal with  
| H : _ |- _ => solve [ inversion H; subst; t ] 
end
|| fail "because the goal is not solvable by inversion.".

Tactic Notation "solve" "by" "inversion" "1" :=
solve_by_inversion_step idtac.
Tactic Notation "solve" "by" "inversion" "2" :=
solve_by_inversion_step (solve by inversion 1).
Tactic Notation "solve" "by" "inversion" "3" :=
solve_by_inversion_step (solve by inversion 2).
Tactic Notation "solve" "by" "inversion" :=
solve by inversion 1.
