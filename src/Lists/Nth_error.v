Require Import List.
Require Import Tactics.
Import ListNotations.

Lemma none_ex_Some: forall {A: Type} x,
  x <> @None A ->
  exists x', x = Some x'.
Proof.
  intros. induction x.
  exists a; auto.
  exfalso; auto.
Qed.

Lemma nth_error_nil : forall (A: Type) n,
  nth_error [] n = @None A.
Proof.
  intros; induction n; auto.
Qed.
Hint Rewrite nth_error_nil.

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
  inversion H. subst. simpl in H0.
  apply nth_error_In in H0. contradiction.
Qed.

Lemma nth_error_same: forall {A: Type} (xs xs': list A),
  (forall i, nth_error xs i = nth_error xs' i) ->
  xs = xs'.
Proof.
  induction xs, xs'; crush. 
  lets ?H: H 0; inversion H0.
  lets ?H: H 0; inversion H0.
  lets ?H: H 0. crush. apply f_equal. apply IHxs. intro i.
  lets ?H: H (S i). crush.
Qed.

Lemma nth_error_split: forall {A: Type} (xs: list A) i x,  
  nth_error xs i = Some x ->
  xs = firstn i xs ++ x :: skipn (S i) xs.
Proof.
  induction xs; intros. rewrite nth_error_nil in H; inversion H.
  destruct i; simpl in *. inversion H. reflexivity.
  apply f_equal.
  eapply IHxs; eauto.
Qed.


Lemma firstn_same : forall {A:Type} (xs ys: list A) i,
  (forall j, j <> i -> nth_error xs j = nth_error ys j) ->
  length xs = length ys ->
  firstn i xs = firstn i ys.
Proof.
  induction xs; eauto; intros. inversion H0.
  destruct ys; crush.
  destruct ys; crush.
  destruct i; crush.
  lets H1: H 0. simpl in H1. crush.
  apply f_equal.
  eapply IHxs; eauto. intros.
  lets H2: H (S j). crush.
Qed.

Lemma skipn_same : forall {A:Type} (xs ys: list A) i,
  (forall j, j <> i -> nth_error xs j = nth_error ys j) ->
  length xs = length ys ->
  skipn (S i) xs = skipn (S i) ys.
Proof.
  induction xs; eauto; intros. inversion H0.
  destruct ys; crush.
  destruct ys; crush.
  destruct i; crush.
  lets ?H: (nth_error_same xs ys). apply H1. intros. 
  lets ?H: H (S i). simpl in *. apply H2; eauto. intuition.
  eapply IHxs; intros; auto.
  lets H2: H (S j); crush.
Qed.
