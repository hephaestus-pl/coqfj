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
Require Export RelationClasses.
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

Hint Resolve beq_id_false_not_eq.
Hint Rewrite beq_id_refl.


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

Lemma in_notin_noteq: forall {A:Type} xs (x1: A) x2,
  In x1 xs ->
  ~In x2 xs ->
  x1 <> x2.
Proof.
  induction xs.
  intros; inversion H.
  intros.
  simpl in *.
  apply Decidable.not_or in H0. destruct H0.
  destruct H.
  rewrite <- H; auto.
  apply IHxs; auto.
Qed.


Fixpoint zipWith {A B C: Type}(f: A -> B -> C) (xs: [A] )(ys: [B]) : [C]:=
match xs,ys with
| nil,_ => nil
| _,nil => nil
| (x::xs'),(y::ys') => (f x y) :: (zipWith f xs' ys')
end.

Fixpoint find_w (n: nat) (key: id) (l: list id) :=
  match l with
    | [] => None
    | (x :: xs) => if beq_id key x 
                    then Some n
                    else find_w (S n) key xs
  end.

Definition find_where := (find_w 0).

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


Lemma nth_error_app_app : forall A (l l': list A) n x,
  nth_error l n = Some x ->
  nth_error (l ++ l') n = Some x.
Proof.
  intros. rewrite nth_error_app1; auto. 
  apply nth_error_Some. intro.
  rewrite H in H0; inversion H0.
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

Lemma nth_error_fst: forall {A: Type} {a: A} l i,
  NoDup (a::l) ->
  nth_error (a::l) i = Some a ->
  i = 0.
Proof.
  intros.
  induction i. auto.
  inversion H. subst. simpl in H0. SearchAbout nth_error.
  apply nth_error_In in H0. contradiction.
Qed.

Lemma find_w_S : forall xs n x i,
  find_w n x xs = Some i ->
  find_w (S n) x xs = Some (S i).
Proof.
  induction xs, n; intros; try (inversion H; auto).
  case beq_id eqn:Beq. inversion H1. simpl in *. 
  apply beq_id_eq in Beq; rewrite Beq. rewrite beq_id_refl; auto.
  simpl. apply beq_id_false_not_eq in Beq.
  rewrite not_eq_beq_id_false in *; auto.
  clear H.
  simpl.
  case beq_id eqn:Beq. inversion H1. eauto. apply IHxs. auto.
Qed.

Lemma findwhere_ntherror : forall xs x i,
  NoDup xs ->
  nth_error xs i = Some x ->
  find_where x xs = Some i. 
Proof. 
  induction xs. intros; rewrite nth_error_nil in H0; inversion H0.
  intros.
  inversion H. clear H. subst. sort.
  unfold find_where; simpl in *.
  case i in *.
    - inversion H0.  rewrite beq_id_refl. auto.
    - simpl in *. case beq_id eqn:Beq. apply beq_id_eq in Beq.
  rewrite Beq in H0. false.
  apply H3. apply nth_error_In with i; auto. apply find_w_S. eapply IHxs; auto.
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

Lemma Forall2_nth_error'(l:list A)(l': list B): forall n x,
  Forall2 P l l' -> 
  nth_error l' n = Some x ->
  exists y, nth_error l n = Some y.
Proof.
  intros. generalize dependent n.
  induction H.
  intros.
  rewrite nth_error_nil in H0; inversion H0.
  intros.
  case n in *.
  simpl. exists x0; auto.
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

Lemma Forall2_exi: forall (Q: B -> B -> Prop) xs ys,
  Forall2 (fun x y => exists y', Q y' y /\ P x y') xs ys ->
  exists ys', Forall2 Q ys' ys /\ Forall2 P xs ys'.
Proof.
  intros. induction H. exists (@nil B); split; constructor.
  destruct H as [z]. destruct H. destruct IHForall2 as [zs]. destruct H2.
  exists (z :: zs). split; constructor; auto.
Qed.

Lemma Forall2_map: forall (f: A -> A) xs ys,  
  Forall2 (fun x => P (f x)) xs ys ->
  Forall2 P (map f xs) ys.
Proof.
  intros. induction H. simpl; auto.
  simpl in *. constructor; auto.
Qed.


End Two_predicate.


Lemma Forall2_trans: forall (A: Type) (P: A -> A -> Prop) xs ys zs,
  Transitive P ->
  Forall2 P xs ys ->
  Forall2 P ys zs ->
  Forall2 P xs zs.
Proof.
  induction xs, ys, zs; intros; auto.
  (* Trivial Cases solved by inversion *)
  - inversion H0. 
  - inversion H1. 
  - inversion H1.

  - inversion H0; inversion H1. subst.
  constructor. transitivity a0; auto.
  eapply IHxs; eauto.
Qed.

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