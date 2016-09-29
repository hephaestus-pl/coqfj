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
  | wf_extds : forall x, In x xs -> ~In x (Dom m) -> NoDup xs -> wf_extd m xs.

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
Lemma extds_nil: forall (B: Type) (xs: list id) (bs: list B),
  @wf_extd id nil xs ->
  [] extds xs : bs = combine xs bs.
Proof.
  induction xs, bs; simpl; auto.
  intros. destruct H. simpl in *. 
  destruct H; inversion H1; subst. 

admit.
  
  inversion H0. 
  unfold extend_list. 
  simpl.
  simpl.

Theorem get_wf_extds: forall (A: Type) (m: env A) xs bs x b i,
  wf_extd m xs ->
  get (m extds xs : bs) x = Some b ->
  nth_error xs i = Some x ->
  nth_error bs i = Some b.
Proof.
  induction m; intros.
  Case "[]".
    unfold extend_list in H0. 
    simpl in *.
 unfold wf_extd in *. simpl in H.
    intros.
  intros.
Admitted.