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
Require Export LibTactics.

(* Identifiers and polymorphic partial maps. *)
Inductive id : Type := 
  Id : nat -> id.

Definition beq_id id1 id2 :=
  match (id1, id2) with
    (Id n1, Id n2) => beq_nat n1 n2
  end.

Theorem beq_id_refl : forall i,
  beq_id i i = true.
Proof.
  intros. destruct i. symmetry.
  apply beq_nat_refl.  Qed.

Theorem beq_id_eq : forall i1 i2,
  beq_id i1 i2 = true -> i1 = i2.
Proof.
  intros i1 i2 H.
  destruct i1. destruct i2. symmetry in H.
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

Definition find_where : id -> list id -> option nat := 
  let fix f (n: nat) (key: id) (l: list id) :=
  match l with
    | [] => None
    | (x :: xs) => if beq_id key x 
                    then Some n
                    else f (S n) key xs
  end in f 0.

Lemma notin_findwhere : forall x xs,
  ~In x xs -> find_where x xs = None.
Proof.
  intros. unfold find_where. generalize 0.
  induction xs.
  intros. reflexivity.
  intros.
  simpl in *. 
  apply Decidable.not_or in H. destruct H.
  unfold find_where.
  simpl. rewrite not_eq_beq_id_false; auto.
Qed.

Lemma in_findwhere : forall x xs,
  In x xs -> exists i, find_where x xs = Some i.
Proof.
  intros. unfold find_where. generalize 0.
  induction xs. 
    - inversion H.
    - intros; simpl. simpl in H. destruct H eqn:Eq.
      rewrite e. rewrite beq_id_refl. eauto. 
      case beq_id. eauto. auto.
Qed.

Lemma none_ex_Some: forall {A: Type} x,
  x <> @None A ->
  exists x', x = Some x'.
Proof.
  intros. induction x.
  exists a; auto.
  exfalso; auto.
Qed.

Lemma nth_error_nil : forall {A: Type} n,
  nth_error [] n = @None A.
Proof.
  intros; induction n; auto.
Qed.

Lemma nth_error_Some' : forall {A: Type} (l: list A)  n,
  n < List.length l ->
  exists x, nth_error l n = Some x.
Proof.
  intros.
  apply <- nth_error_Some in H.
  apply none_ex_Some; auto.
Qed.

Lemma nth_error_In': forall {A:Type} (l: list A) x,
  In x l ->
  exists n, nth_error l n = Some x.
Proof.
  intros.
  induction l. simpl in H; contradiction.
  simpl in *. destruct H.
  rewrite H; simpl. exists 0; auto.
  destruct IHl as [n]; auto.
  exists (S n); auto.
Qed.

Lemma nth_error_same_len : forall {A B:Type} (l:list A) (l': list B) n x,
  length l = length l' ->
  nth_error l n = Some x ->
  exists y, nth_error l' n = Some y.
Proof.
  induction l, l'; intros.
  rewrite nth_error_nil in H0; inversion H0.
  simpl in H; inversion H.
  simpl in H; inversion H.
  intros; simpl in *.
  case n in *. exists b; auto.
  simpl in *.
  eapply IHl; eauto.
Qed.

Lemma nth_error_dec : forall {A: Type} l n,
  {x | nth_error l n = Some x} + {nth_error l n = @None A}.
Proof.
  intros; generalize dependent n.
  induction l.
  right; apply nth_error_nil. intro.
  case n in *. left; exists a; auto.
  simpl. apply IHl.
Defined.

Lemma findwhere_ntherror : forall x xs i,
  find_where x xs = Some i <-> nth_error xs i = Some x.
Admitted.


Definition partial_map (A:Type) := id -> (option A).
Definition empty {A:Type} : partial_map A := fun _ => None.

(* Adds an element at the head of the map *)
Definition update {A:Type} (m : partial_map A)
(x : id) (v : A) :=
fun x' => if beq_id x x' then (Some v) else m x'.

(* Adds an element at the tail of the map *)
Definition extend {A:Type} (m : partial_map A) (x : id) (v : A) (x': id) :=
    match m x' with
     | None => if beq_id x x' then Some v else None
     | otherwise => if beq_id x x' then None else m x'
    end.
Notation " m 'extd' id ':' val" := (extend m id val) (at level 20, id at next level).


(* Extends a list of element at the tail of the map *)
Fixpoint extend_list {A:Type} (m : partial_map A) (ids : list id) (vals : list A) :=
match ids, vals with
 | (x::xs), (v::vs) => extend_list (extend m x v) xs vs
 | _, _ => m
end.

Notation " m 'extds' ids ':' vals" := (extend_list m ids vals) (at level 20, ids at next level).

Lemma update_eq : forall A (m: partial_map A) x v,
(update m x v) x = Some v.
Proof.
intros A m x v. unfold update. rewrite beq_id_refl with (i:= x).
reflexivity.
Qed.


Theorem update_neq : forall (X:Type) v x1 x2
(m : partial_map X),
x2 <> x1 -> 
(update m x2 v) x1 = m x1.
Proof.
intros X v x1 x2 m H.
unfold update. rewrite not_eq_beq_id_false. reflexivity.
apply H. Qed.

Lemma update_shadow : forall A (m: partial_map A) v1 v2 x,
(update (update m x v1) x v2) x = Some v2.
Proof.
  intros A m v1 v2 x.
  unfold update.
  rewrite beq_id_refl; auto.
Qed.

Lemma extend_wf : forall A (m: partial_map A) v x,
m x <> None ->
(extend m x v) x = None.
Proof.
  intros A m v x1 H. unfold extend.
  apply none_ex_Some in H. destruct H. 
  rewrite H; auto.
  rewrite beq_id_refl; eauto.
Qed.

Lemma extend_list_ill: forall A (m: partial_map A) x xs,
  m x = (m extds xs : nil) x.
Proof.
  intros.
  induction xs.
  simpl; auto.
  simpl. auto.
Qed.

Lemma extend_list_not_shadow: forall A (m: partial_map A) x xs ds,
~In x xs ->
(m extds xs : ds) x = m x.
Proof.
  intros; gen xs ds m.
  induction xs, ds; auto.
  apply not_in_cons in H. destruct H.
  simpl.
  intros.
  rewrite IHxs; auto.
  unfold extend.
  destruct (m x) eqn:Heq;
  rewrite not_eq_beq_id_false; auto.
Qed.

Lemma extend_neq : forall A (m: partial_map A) v x x0,
x <> x0 ->
(extend m x v) x0 = m x0.
Proof.
  intros A m v x1 x2 H. unfold extend.
  case (m x2);
  rewrite not_eq_beq_id_false; auto.
Qed.

Section Ref.

Class Referable (a: Type) :={
ref : a -> id;
find: id -> list a -> option a := 
let fix f (key: id) (l: list a) :=
match l with
  | [] => None
  | (x :: xs) => if beq_id key (ref x) 
                  then Some x
                  else f key xs
end in f;
}.


Inductive findi {A: Type} {R: @Referable A} : id -> list A -> A -> Prop:=
| find_head : forall x xs, findi (ref x) (x :: xs) (x)
| find_step : forall k1 k2 l x, 
  k1 <> ref k2 -> findi k1 l x -> findi k1 (k2 :: l) x.



Lemma find_deterministic: forall (A: Type) (R: @Referable A) d (k1: id) (x1 x2: option A),
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

Lemma findi_diff: forall (A: Type) (R: @Referable A) k a v l,
k = ref a -> a <> v -> ~findi k (a :: l) v.
Proof.
  intros.
  intro.
  inversion H1; subst.
  apply H0; auto.
  apply H5; auto.
Qed.

Lemma find_iff_findi: forall (A: Type) (R: @Referable A) d (k1: id) (x1: A),
find k1 d = Some x1 <-> findi k1 d x1.
Proof.
  intros. split.
  intro H. 
  induction d.
  inversion H.

  remember (ref a) as r.
  destruct eq_id_dec with (i1:= k1) (i2:= r); rewrite Heqr in *; clear Heqr.
  rewrite e in H; simpl in H. rewrite beq_id_refl in H.
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
  rewrite beq_id_refl. auto.
  unfold find.
  rewrite not_eq_beq_id_false; auto.
Qed.

End Ref.

(** Forall for two-fold relations **)


Section Two_predicate.

Variables A B: Type.
Variable P: A -> B -> Prop.

Lemma Forall2_len: forall xs ys,
  Forall2 P xs ys -> length xs = length ys.
Proof.
  intros.
  induction H. auto.
  simpl. rewrite IHForall2; auto.
Qed.

Lemma Forall2_nth_error(l:list A)(l': list B): forall n x,
  Forall2 P l l' -> 
  nth_error l n = Some x ->
  exists y, nth_error l' n = Some y.
Proof.
  intros. generalize dependent n.
  induction H.
  intros.
  rewrite nth_error_nil in H0; inversion H0.
  intros.
  case n in *.
  simpl; exists y; auto.
  simpl in *.
  apply IHForall2; auto.
Qed.

Lemma Forall2_forall (l:list A)(l': list B): forall n x y,
  Forall2 P l l' -> 
    nth_error l  n = Some x ->
    nth_error l' n = Some y -> P x y.
Proof.
  intros n x y H.
  generalize dependent n.
  induction H.
  intros.
  rewrite nth_error_nil in H. inversion H.
  intros.
  case n in *; simpl in H1, H2. inversion H1. inversion H2. rewrite <- H4, <- H5; auto.
  apply IHForall2 with n; auto.
Qed.

End Two_predicate.

(** * From Basics.v *)

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