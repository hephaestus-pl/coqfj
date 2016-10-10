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

Definition Dom {A:Type} (m: env A):= map fst m.
(*
Definition wf_extd {A: Type} (m:env A) xs := (forall x, In x xs -> ~In x (Dom m)) /\ (NoDup xs).
*)
Inductive wf_extd {A: Type} (m:env A) xs : Prop :=
  | wf_extds : (forall x, In x xs -> ~In x (Dom m)) -> NoDup xs -> wf_extd m xs.

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

Lemma extds_not_shadow: forall {A: Type} xs (m: env A) x bs,
  In x (Dom m) ->
  get (m extds xs : bs) x = get m x.
Proof.
Admitted.


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
  induction m.
  Case "nil".
    split; constructor; auto.
  Case "a :: m". 
    intros; split; intro.
    SCase "->".
      inversion H. destruct a; subst. simpl.
      case beq_id eqn:Heq.
      SSCase "x = i".
        constructor; auto.
      SSCase "x <> i". 
        simpl. constructor. intro. apply H2. simpl.
        apply <- notin_extd; eauto. 
        apply beq_id_false_not_eq; auto.
        apply IHm; auto.
    SCase "<-".
      destruct a; simpl in *.
      case beq_id eqn:Heq.
      SSCase "x = i". 
        simpl in *; assumption.
      SSCase "x <> i".
        simpl in *. inversion H. subst.
        constructor.
        intro. apply H2. apply notin_extd; auto using (beq_id_false_not_eq).
        apply IHm in H3; auto.
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

Lemma get_wf_extd: forall (A: Type) (m: env A) x b i,
  wf_extd m [x] ->
  get (m extd x : b) x = Some b ->
  nth_error [x] i = Some x ->
  nth_error [b] i = Some b.
Proof.
Admitted.
Print combine.

(*
Lemma extds_nil: forall (B: Type) (xs: list id) (bs: list B),
  @wf_extd id nil xs ->
  [] extds xs : bs = combine xs bs.
Proof.
  induction xs, bs; simpl; auto.
  intros. destruct H. simpl in *.  
  edestruct H. inversion H1. esubst. 
Admitted.
*)

Lemma get_noteq: forall (A:Type) (m: env A) x' x a,
  x' <> x ->
  get ((x', a) :: m) x = get m x.
Proof.
  induction m.
  intros; simpl; auto.
  rewrite not_eq_beq_id_false; auto.
  intros. simpl.
  rewrite not_eq_beq_id_false; auto.
Qed.

Lemma get_in_Dom: forall (A: Type) (b: A) m x,
  get m x = Some b ->
  In x (Dom m).
Proof.
  induction m.
  intros. inversion H.
  destruct a.
  intros.
  unfold Dom. simpl.
  destruct eq_id_dec with x i. left; auto.
  right.
  apply IHm.
  rewrite get_noteq in H; auto.
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

Lemma wf_weak: forall (A:Type) (m: env A) (x: id) xs,
  wf_extd m (x::xs) ->
  wf_extd m xs.
Proof.
  induction m.
  intros. destruct H.
  constructor; auto. intros.
  inversion H0; auto.
  destruct a; intros.
  constructor.
  intros.
  simpl.
  intro.
  destruct H. destruct H1. eapply H.
  simpl. right. apply H0.
  rewrite H1; simpl; auto.
  eapply H. simpl. right. exact H0.
  simpl. right; auto.
  destruct H. inversion H0; auto.
Qed.


Lemma get_extd_notin_Dom: forall (A: Type) (b0: A) m x xs bs,
  ~In x (Dom m) ->
  get (m extds (x::xs) : (b0 ::bs)) x = Some b0.
Proof.
  induction m. simpl.
  intros. rewrite extds_not_shadow. simpl. rewrite beq_id_refl. auto.
  simpl; auto. 
  destruct a; simpl; auto.
  simpl in *.
  SearchAbout not or. intros.
  apply Decidable.not_or in H. destruct H.
  rewrite not_eq_beq_id_false; auto.
Admitted.


Lemma get_extds_notin_head: forall (A: Type) (m: env A) xs bs x' x b,
  x <> x' ->
  get (m extds (x':: xs) : (b::bs)) x = get (m extds xs : bs) x.
Proof.
  intros; simpl.
Admitted.

Theorem get_wf_extds: forall (A: Type)  xs (m: env A) bs x b i,
  wf_extd m xs ->
  get (m extds xs : bs) x = Some b ->
  nth_error xs i = Some x ->
  nth_error bs i = Some b.
Proof.
  induction xs. 
  intros. inversion H. rewrite nth_error_nil in H1. inversion H1.
  intros.
  destruct H.
  inversion H2.
  assert (i=0).
    eapply nth_error_fst. exact H2. rewrite <- H3 in H1; auto.
  case bs in *. simpl in H0. 
  apply get_in_Dom in H0.
  false. apply H with x; auto. rewrite <- H3. apply nth_error_In with i;auto.
  subst.
  
  rewrite H6; simpl.
  rewrite H5 in H0.
  rewrite get_extd_notin_Dom in H0; auto.

  case i in *.
  case bs in *. simpl in H0.
  apply get_in_Dom in H0. contradiction.
  simpl in *.
  inversion H1. rewrite H7 in H2.
  inversion H2. subst. contradiction.
  
  case bs in *.
  simpl in *. 
  apply get_in_Dom in H0; contradiction.
  inversion H2. subst.
  
  simpl in *.
  eapply in_notin_noteq in H5; eauto.
  apply IHxs with m x; auto.
  apply wf_weak with a.
  constructor; auto.

  erewrite <- get_extds_notin_head; eauto.

Qed.