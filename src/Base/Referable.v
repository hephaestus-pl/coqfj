
Require Import List.
Require Import Tactics.
Require Import Lists.
Require Import Id.
Import ListNotations.


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

Lemma find_w_S : forall xs n x i,
  find_w n x xs = Some i ->
  find_w (S n) x xs = Some (S i).
Proof.
  induction xs, n; intros; try (inversion H; auto).
  case beq_id eqn:Beq. inversion H1. simpl in *. 
  apply beq_id_eq in Beq; rewrite Beq. rewrite beq_id_refl; auto.
  simpl. apply beq_id_false_neq in Beq.
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
  inversion H. clear H. subst.
  unfold find_where; simpl in *.
  case i in *.
    - inversion H0.  rewrite beq_id_refl. auto.
    - simpl in *. case beq_id eqn:Beq. apply beq_id_eq in Beq.
  rewrite Beq in H0. apply False_ind.
  apply H3. apply nth_error_In with i; auto. apply find_w_S. eapply IHxs; auto.
Qed.

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
end in f;
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

Lemma find_dec: forall {A: Set} {R: @Referable A} d (k: id),
  {exists x, find k d = Some x} + {find k d = None}.
Proof.
  intros.
  induction d. crush.
  simpl.
  destruct eq_id_dec with (ref a) k. 
  rewrite e. rewrite beq_id_refl; eauto.
  rewrite not_eq_beq_id_false; auto.
Qed.

End Ref.


Instance Referable_id : Referable id :={
  ref id := id
}.

Lemma Forall_find: forall {A: Set} {R: @Referable A} P xs id x,
  Forall P xs ->
  find id xs = Some x ->
  P x.
Proof.
Admitted.

Lemma nth_error_find {A: Set} : forall {R: @Referable A} x xs,
  In x xs -> 
  (exists i, find i xs = Some x).
Proof.
  induction xs.
  intros; inversion H.
  simpl; intros. destruct H.
  rewrite H. exists (ref x). rewrite beq_id_refl; auto.
  destruct IHxs; auto.
  destruct eq_id_dec with (Some a) (Some x).
 exists (ref a); auto. rewrite beq_id_refl. 
Admitted.

Module Refs.
Notation "'refs' x":= (map ref x) (at level 30).
End Refs.
Export Refs.
