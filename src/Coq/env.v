Require Import Metatheory.
Import List.
Import ListNotations.

(* Add a single element to the end of the list *)
Fixpoint snoc {A: Type} (x: A) (xx: list A) : list A :=
 match xx with 
 | nil       => x :: nil
 | y :: xs'  => y :: snoc x xs'
 end.



Definition env (A:Type) := list (id * A).

Fixpoint get {A: Type} (m: env A) (x: id): option A :=
  match m with
  | nil => None
  | (x1, a) :: ms =>
    if (beq_id x x1) then Some a
    else get ms x
  end.

Notation "xs :> x" := (snoc x xs) (at level 62, right associativity).
Notation " m 'extd' id ':' val" := (m :> (id, val)) (at level 20, id at next level).
Notation " m 'extds' ids ':' vals" := (m ++ (combine ids vals)) (at level 20, ids at next level).

Definition Dom {A:Type} (m: env A):= map fst m.

Inductive wf_extd {A: Type} (m:env A) xs : Prop :=
  | wf_extds : (forall x, In x xs -> ~In x (Dom m)) -> NoDup xs -> wf_extd m xs.

Lemma cons_snoc_empty
 :  forall A (x: A)
 ,  x :: nil = nil :> x.
Proof. 
 auto.
Qed.
Hint Resolve cons_snoc_empty.


Lemma snoc_app
 :  forall A (xs: list A) (x: A) (ys: list A)
 ,  (ys ++ x :: xs) = ((ys :> x) ++ xs).
Proof.
  induction ys. simpl in *; auto.
  simpl in *. apply f_equal. auto.
Qed.
Hint Resolve snoc_app.

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

Lemma extend_nshadow': forall {A: Type} (m: env A) x a,
  In x (Dom m) ->
  get (m extd x : a) x = get m x.
Proof.
  induction m in *; intros. inversion H. 
  destruct a.  destruct H. simpl in *. 
  rewrite H.
  rewrite beq_id_refl; auto.
  simpl. case beq_id; auto.
Qed.

Lemma extend_nshadow: forall {A: Type} (m: env A) x a b,
  get m x = Some b ->
  get (m extd x : a) x = Some b.
Proof.
  induction m in *; intros. inversion H. 
  destruct a. simpl in *.
  case beq_id in *; auto.
Qed.

Lemma extends_nshadow: forall {A: Type} (m: env A) x xs bs b,
  get m x = Some b ->
  get (m extds xs : bs) x = Some b.
Proof.
  induction m in *; intros. inversion H. 
  destruct a. simpl in *.
  case beq_id in *; auto.
Qed.

Theorem extend_neq : forall (X:Type) (m : env X) v x1 x2,
x2 <> x1 -> 
get (m extd x2 : v) x1 = get m x1.
Proof.
  induction m; intros. simpl. rewrite not_eq_beq_id_false; auto.
  destruct a; simpl.
  case beq_id eqn:Heq; case (beq_id x1 i) eqn:Heq1; auto.
Qed.

Lemma get_nHead: forall (A:Type) (m: env A) x' x a,
  x' <> x ->
  get ((x', a) :: m) x = get m x.
Proof.
  intros; simpl; auto.
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
  rewrite get_nHead in H; auto.
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

Lemma get_In_Dom: forall (A: Type) (m: env A) x,
  In x (Dom m) ->
  exists x', get m x = Some x'.
Proof.
  induction m.
  simpl; intros. contradiction.
  intros; simpl.
  simpl in H. destruct a. simpl in *.
  destruct H. rewrite H; rewrite beq_id_refl; eexists; auto.
  destruct IHm with x; auto.
  case beq_id. eauto. eexists; eauto.
Qed.


Lemma get_extd_m: forall (A: Type) (m: env A) x' b' x,
  In x (Dom m) ->
  get (m extd x' : b') x = get m x.
Proof.
  induction m. simpl; auto. intros; contradiction.
  intros; simpl; auto.
  destruct a. simpl in *.
  destruct H. rewrite H; rewrite beq_id_refl. auto.
  case beq_id. 
  auto. apply IHm; auto.
Qed.

Lemma In_Dom_weak: forall (A: Type) (m: env A) x x' b,
  In x (Dom m) ->
  In x (Dom (m extd x' : b)).
Proof.
  induction m.
  simpl; intros; contradiction.
  destruct a.
  simpl; intros.
  destruct H.
  rewrite H.
  simpl. left; auto.
  simpl. right. apply IHm; auto.
Qed.

Lemma get_extds_m: forall (A: Type) xs bs (m: env A) x,
  In x (Dom m) ->
  get (m extds xs : bs) x = get m x.
Proof.
  induction xs; intros; auto. simpl. 
  rewrite app_nil_r. auto.
  case bs in *; auto. simpl. rewrite app_nil_r; auto. simpl.
  rewrite snoc_app.
  rewrite IHxs with bs (m extd a : a0) x. apply get_extd_m. assumption.
  apply In_Dom_weak; auto.
Qed.

Theorem get_wf_extds: forall (A: Type)  xs (m: env A) bs x b i,
  NoDup xs ->
  get (m extds xs : bs) x = Some b ->
  nth_error xs i = Some x ->
  nth_error bs i = Some b.
Proof.
  induction xs. intros. rewrite nth_error_nil in H1. inversion H1.
  intros.
  case bs in *. simpl in *. rewrite app_nil_r in H0.  
  simpl.
simpl in *.
Admitted.

Lemma get_notIn_Dom: forall (A: Type) (m: env A) x,
  ~ In x (Dom m) ->
  get m x = None.
Proof.
Admitted.

Lemma extend_nshadow': forall {A: Type} (m: env A) x a b,
  In x (Dom m) ->
  get (m extd a : b) x = get m x.
Proof.
Admitted.

Lemma extds_nshadow_In_Dom: forall {A: Type} xs (m: env A) x bs,
  In x (Dom m) ->
  get (m extds xs : bs) x = get m x.
Proof.
  induction xs, bs; intros; simpl; auto.
  rewrite IHxs with (m extd a : a0) x bs.
  rewrite extend_nshadow'; auto.

  
Admitted.

Lemma extends_nshadow_nIn_m: forall A (m: env A) x xs bs,
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
  induction m.
  intros; simpl in *. rewrite beq_id_refl in H0. inversion H. case i in *. auto. simpl in H1.
  rewrite nth_error_nil in H1. inversion H1.
  intros; simpl in *.
  destruct a. inversion H.
  simpl.
Admitted.




Lemma get_extd_not_m: forall (A: Type) (m: env A) x' b' x,
  ~ In x (Dom m) ->
  get (m extd x' : b') x = get ([] extd x' : b') x.
Proof.
  induction m.
  simpl; intros. auto.
  simpl; intros.
  apply Decidable.not_or in H. destruct H.
  destruct a. simpl in *.
  case beq_id eqn:Beq.
  simpl. rewrite not_eq_beq_id_false; auto.
  apply beq_id_eq in Beq. rewrite <- Beq in H.
  rewrite not_eq_beq_id_false; auto.
  apply get_notIn_Dom; auto.

  apply beq_id_false_not_eq in Beq.
  destruct eq_id_dec with x x'.
  simpl. 
  rewrite not_eq_beq_id_false. rewrite IHm; auto. auto. simpl.
  rewrite not_eq_beq_id_false. apply IHm; auto. auto.
Qed.


Lemma get_extd_notin_Dom_nil: forall (A: Type) (b0: A) x xs bs,
  get ([] extds (x::xs) : (b0 ::bs)) x = Some b0.
Proof.
  intros. simpl.
  rewrite get_extds_m. simpl. autorewrite with core; auto.
  simpl; auto.
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
  inversion H2; subst.
  case i in *; case bs in *. simpl in *.
  false. apply H with x. left; inversion H1. auto.
  eapply get_in_Dom; eauto.
  inversion H1. subst.
  rewrite get_extd_notin_Dom in H0; auto.
  eapply H; auto.
  simpl. left; auto. 
  
  simpl in *. false; apply H with x. right.
  eapply nth_error_In. eauto.
  eapply get_in_Dom; eauto.

  simpl.
  eapply IHxs.
  eapply wf_weak; constructor; eauto.
  rewrite get_extds_notin_head in H0. eauto.
  simpl in *.
  apply nth_error_In in H1.
  apply in_notin_noteq with xs; auto.

  simpl in H1; exact H1.
Qed.