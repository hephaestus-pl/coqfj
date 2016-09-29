Require Import Metatheory.
Import List.
Import ListNotations.


Definition env (A:Type) := list (id * A).

Fixpoint get {A: Type} (m: env A) (x: id): option A :=
  match m with
  | nil => None
  | (x1, a) :: ms =>
    if (beq_id x x1) then Some a
    else get ms x
  end.

Fixpoint extend {A: Type} (m: env A) (x: id) (a: A) :=
  match m with
  | [] => [(x,a)]
  | (x',a') :: ms => if beq_id x x' then m else (x',a') :: extend ms x a
  end.
Notation " m 'extd' id ':' val" := (extend m id val) (at level 20, id at next level).

Fixpoint extend_list {B: Type} (m: env B) (xs: list id) (bs: list B): (env B) :=
  match xs, bs with
  | x :: xs', b :: bs' => extend_list (extend m x b) xs' bs'
  | _ , _ => m
  end.
Notation " m 'extds' ids ':' vals" := (extend_list m ids vals) (at level 20, ids at next level).

Definition Dom {A:Type} (m: @env A):= map fst m.

Lemma update_not_shadow: forall {A: Type} (m: env A) x a,
  In x (Dom m) ->
  (m extd x : a) = m.
Proof.
  intros; induction m in *. inversion H. 
  destruct a0.  simpl in *. destruct H.
  rewrite H; rewrite beq_id_refl. auto.
  case beq_id; auto.
  rewrite IHm; auto.
Qed.


Theorem extend_neq : forall (X:Type) (m : env X) v x1 x2,
x2 <> x1 -> 
get (extend m x2 v) x1 = get m x1.
Proof.
  induction m; intros. simpl. rewrite not_eq_beq_id_false; auto.
  destruct a; simpl.
  case beq_id eqn:Heq; case (beq_id x1 i) eqn:Heq1;
  unfold get; rewrite Heq1; auto.
Qed.

Lemma notin_extd: forall (A: Type) (m: env A) x i v,
  x <> i -> 
  (In i (Dom m) <-> 
   In i (Dom (m extd x : v))).
Proof.
  induction m; intros; split; intros; simpl in *; auto. 
  - inversion H0; [contradiction | assumption].
  - intros. destruct a. simpl in *.
    case beq_id. simpl in *. auto. simpl.
    destruct H0. auto. right. apply IHm; auto.
  - destruct a. case beq_id in H0; simpl in *. auto.
    destruct H0. left; auto. right; apply IHm in H0; auto.
Qed.

Theorem extend_nodup: forall (A: Type) (m: env A) x v,
NoDup (Dom m) <-> NoDup (Dom (extend m x v)).
Proof.
  induction m; split; intros; simpl; auto; unfold extend. simpl.
  constructor; auto.
  constructor; auto.
  inversion H. destruct a; subst.
  simpl. case beq_id eqn:Heq. constructor. auto. auto. simpl.
  constructor. intro. apply H2. simpl.
  apply <- notin_extd; eauto. apply beq_id_false_not_eq; auto.
  apply IHm; auto. 
  destruct a; simpl in *.
  inversion H.
  case beq_id eqn:Heq in H1. simpl in *. rewrite <- H1; constructor. inversion H1.

  destruct x0; subst.
  destruct eq_id_dec with x i. 
  rewrite e in H0. rewrite beq_id_refl in H0. simpl in H0. rewrite <- H0; constructor; auto.
  rewrite not_eq_beq_id_false in H0; auto.
  inversion H0. subst.  
  rewrite not_eq_beq_id_false in H; auto. simpl in H.
  apply IHm in H2.
 constructor. intro. apply H1.
  apply notin_extd; eauto. auto.
Qed.

Lemma extend_list_not_shadow: forall A (m: env A) x xs bs,
  ~In x xs ->
  get (m extds xs : bs) x = get m x.
Proof.
  intros; gen xs bs m x.
  induction xs, bs; auto; intros.
  apply not_in_cons in H. destruct H.
  simpl. rewrite IHxs; auto.
  case m. simpl. rewrite not_eq_beq_id_false; auto. intros.
  rewrite extend_neq; auto.
Qed.

Theorem extend_list_nodup: forall (A: Type) xs bs (m: env A),
NoDup (Dom m) <-> NoDup (Dom (m extds xs : bs)).
Proof.
  induction xs, bs; auto; intros; try (simpl; split; intro; assumption).
  split; intro; simpl in *.
  -  apply IHxs.
     apply extend_nodup.
     assumption.
 -  apply IHxs in H.
    eapply extend_nodup.
    eauto.
Qed.