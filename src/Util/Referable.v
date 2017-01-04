
Require Import List.
Require Import Tactics.
Require Import Lists.
Require Import Id.
Import ListNotations.
Hint Constructors NoDup.


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

Class Referable (A: Set) :={
ref : A -> id;
find: id -> list A -> option A := 
  let fix f (key: id) (l: list A) :=
  match l with
    | [] => None
    | (x :: xs) => if beq_id key (ref x) 
                    then Some x
                    else f key xs
  end in f;
}.



Lemma find_deterministic: forall `{R: Referable} d (k1: id) x1 x2,
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

Lemma find_dec: forall `{R: Referable} d (k: id),
  {exists x, find k d = Some x} + {find k d = None}.
Proof.
  intros.
  induction d. crush.
  simpl.
  destruct beq_id_dec with (ref a) k. 
  rewrite e. rewrite beq_id_refl; eauto.
  rewrite not_eq_beq_id_false; auto.
Qed.

End Ref.

Module Refs.
Notation "'refs' x":= (map ref x) (at level 30).
End Refs.
Export Refs.


Section Findi.

Inductive findi {A: Set} {R: @Referable A} : id -> list A -> A -> Prop:=
| find_head : forall x xs, findi (ref x) (x :: xs) (x)
| find_step : forall k1 k2 l x, 
  k1 <> ref k2 -> findi k1 l x -> findi k1 (k2 :: l) x.



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
  destruct beq_id_dec with (i1:= k1) (i2:= r); rewrite Heqr in *; clear Heqr.
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



Lemma findi_none: forall (A: Set) (R: @Referable A) k l,
  find k l = None <-> (forall x, ~findi k l x).
Proof.
  intros.
  split. intro. intros. intro. destruct H0. simpl in H.
  rewrite beq_id_refl in H. inversion H.
  apply <- find_iff_findi in H1.
  simpl in H. rewrite not_eq_beq_id_false in H; auto. rewrite H1 in H; inversion H.
  intros. induction l. reflexivity.
  simpl in *. destruct beq_id_dec with k (ref a).
  false. subst. destruct H with a. apply find_head.
  rewrite not_eq_beq_id_false; auto. apply IHl.
  intros. intro. destruct H with x. apply find_step; auto.
Qed.

End Findi.


Lemma find_ref_inv: forall `{R: Referable} d (k: id) x,
  find k d = Some x ->
  ref x = k.
Proof.
  intros. apply find_iff_findi in H. induction H; crush.
Qed.


Lemma find_in: forall `{R: Referable} id xs x,
  find id xs = Some x ->
  In x xs.
Proof.
  intros. 
  apply find_iff_findi in H. induction H; crush.
Qed.

Lemma Forall_find: forall `{R: Referable} P xs id x,
  Forall P xs ->
  find id xs = Some x ->
  P x.
Proof.
  intros. 
  eapply Forall_forall in H; eauto. 
  eapply find_in; eauto.
Qed.

Lemma in_imp_inref: forall {T} {H: Referable T} (xs:list T) x,
  In x xs ->
  In (ref x) (refs xs).
Proof.
  induction xs; crush.
Qed.

Lemma ref_noDup_nth_error: forall {T} {H: Referable T} (xs:list T) i i1 x x1,
  nth_error xs i = Some x ->
  nth_error xs i1 = Some x1 ->
  NoDup (refs xs) ->
  ref x = ref x1 ->
  x = x1.
Proof.
  induction xs; crush.
  destruct i; destruct i1; simpl in *; crush.
  rewrite H3 in H2. inversion H2. subst. clear H3.
  false. apply H5. apply nth_error_In in H1. apply in_imp_inref; eauto.
  rewrite <- H3 in H2. inversion H2. subst. false.
  apply H5. apply nth_error_In in H0. apply in_imp_inref; eauto.
  inversion H2.
  eapply IHxs; eauto.
Qed.