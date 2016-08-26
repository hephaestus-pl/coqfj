Require Export Syntax.
Import Arith.

Lemma arg_dec: forall a1 a2: Argument,
  {a1 = a2} + {a1 <> a2}.
Proof.
  intros; destruct a1, a2.
  destruct eq_id_dec with (i1 := i) (i2 := i0).
  rewrite e; left; reflexivity.
  right; intro.
  destruct n. inversion H. reflexivity.
Qed.

Lemma farg_dec: forall f1 f2: FormalArg,
  {f1 = f2} + {f1 <> f2}.
Proof.
  intros; destruct f1, f2.
  destruct eq_id_dec with (i1 := i) (i2 := i0);
  destruct eq_id_dec with (i1 := c) (i2 := c0); 
  try (right; intro; destruct n; inversion H; reflexivity).
  rewrite e, e0; left; auto.
Qed.

Lemma assnmt_dec: forall a1 a2: Assignment,
  {a1 = a2} + {a1 <> a2}.
Proof.
  intros; destruct a1, a2.
  destruct eq_id_dec with (i1 := i) (i2 := i1);
  destruct eq_id_dec with (i1 := i0) (i2 := i2);
  try (right; intro H; destruct n; inversion H; auto).   
  rewrite e, e0; left; auto.
Qed.

Lemma field_dec: forall f1 f2 : FieldDecl,
  {f1 = f2} + {f1 <> f2}.
Proof.
  intros; destruct f1, f2.
  destruct eq_id_dec with (i1 := i) (i2 := i0);
  destruct eq_id_dec with (i1 := c) (i2 := c0); 
  try (right; intro; destruct n; inversion H; reflexivity).
  rewrite e, e0; left; auto.
Qed.

Lemma exp_dec: forall e1 e2 : Exp,
  {e1 = e2} + {e1 <> e2}.
Proof.
  intros; destruct e1, e2; try solve [right; intro H; inversion H].
Admitted.

Lemma mdecl_dec: forall m1 m2: MethodDecl,
  {m1=m2} + {m1 <> m2}.
Proof.
  intros.
  destruct m1, m2.
  destruct eq_id_dec with (i1 := c) (i2 := c0);
  destruct exp_dec with (e1:= e) (e2:= e0);
  destruct eq_id_dec with (i1:= i0) (i2:= i);
  destruct list_eq_dec with (A:= FormalArg) (l:= l) (l':= l0); try(exact farg_dec); try (right; intro; destruct n; inversion H; reflexivity).

  left; rewrite e1, e2, e3, e4; reflexivity.
Qed. 

Lemma A11: forall m D C Cs C0,
          C <: D ->
          mtype(m,D) = Cs ~> C0 ->
          mtype(m,C) = Cs ~> C0.
Proof with eauto.
  intros m D C Cs C0 H.
  subtype_cases (induction H) Case...
  Case "S_Decl".
    intro H0.
    inversion H0;
    (destruct in_dec with (A:= MethodDecl) (a := MDecl C0 m fargs e) (l:= mds);
      [ exact mdecl_dec
      | eapply mty_ok; eauto 
      | eapply mty_no_override; eauto
    ]).
Qed.


Lemma weakening: forall Gamma e x C D,
  Gamma |- e : C ->
  Gamma extd x : D |- e : C.
Proof with eauto.
  intros.
  typing_cases (induction H using ExpTyping_ind') Case; try (solve [econstructor; eauto]).
  Case "T_Var".
    constructor.
    destruct eq_id_dec with x x0; subst.
    apply extend_not_shadow; assumption.
    rewrite <- H.
    apply extend_neq; auto.
Qed.

Lemma fields_obj_nil: forall f,
  fields Object f -> f = nil.
Proof.
  intros.
  remember Object.
  induction H; auto.
  rewrite Heqc in H.
  rewrite sane_CT in H.
  inversion H.
Qed.

Lemma fields_det: forall C f1 f2,
  fields C f1 ->
  fields C f2 ->
  f1 = f2.
Proof.
  intros.
  generalize dependent f1.
  fields_cases (induction H0) Case; intros.
  Case "F_Obj".
    apply fields_obj_nil; auto.
  Case "F_Decl".
    inversion H1.
    subst.
    rewrite sane_CT in H. inversion H.
    subst.
    rewrite H in H2.
    inversion H2. subst.
    rewrite IHfields with fs'0; auto.
Qed.

Lemma subst_list_eq_size : forall (xs:[Var]) (ds:[Exp]) e e',
  [; ds \ xs ;] e = e' ->
  length ds = length xs.
Proof.
  intros.
  induction H.
  auto. simpl.
  apply Nat.succ_inj_wd. auto.
Qed.


Lemma subst_list_det : forall ds xs e e1 e2,
  [; ds \ xs ;] e = e1 ->
  [; ds \ xs ;] e = e2 ->
  e1 = e2.
Proof.
  intros.
  generalize dependent e2.
  induction H.
  intros. inversion H0; auto.
  intros. apply IHsubst_list.
  inversion H1; subst. auto.
Qed.

Lemma subst_exi_len : forall ds xs e e',
  [; ds \ xs ;] e = e' ->
  exists n, n <= length ds /\ n <= length xs.
Proof.
  intros.
 induction H. simpl; eexists; split; auto. 
  destruct IHsubst_list as [n]. destruct H1.
  exists (S n); simpl; auto using le_n_S.
Qed.

Lemma subst_nth_error: forall ds xs e e' i xi,
  [; ds \ xs ;] e = e' ->
  nth_error xs i = Some xi ->
  exists di, nth_error ds i = Some di.
Proof.
  intros.
  generalize dependent i.
  induction H. intros; simpl. rewrite nth_error_nil in H0. inversion H0.
  intros. case i in *. simpl; exists e'; auto.
  simpl in *. apply IHsubst_list; auto.
Qed.


Lemma var_subst_notin : forall (xs: [Var]) (ds: list Exp) (x: Var),
  length ds = length xs ->  
  ~ In x xs ->
  [; ds \ xs ;] (ExpVar x) = (ExpVar x).
Proof.
  intros.
  generalize dependent ds.
  induction xs, ds; intros. constructor.
  inversion H.
  inversion H. 
  apply not_in_cons in H0; destruct H0.
  apply Subst_cons with (ExpVar x).
  unfold subst. rewrite not_eq_beq_id_false; auto.
  apply IHxs. auto.
  simpl in H. apply Nat.succ_inj_wd; auto.
Qed.

Lemma free_in_context : forall x e C Gamma,
  appears_free_in x e ->
  Gamma |- e : C ->
  exists C', Gamma x = Some C'.
Proof with eauto.
  intros x e C Gamma H. generalize dependent C.
  afi_cases (induction H) Case; intros.
  Case "afi_var".
    inversion H; subst. exists C; auto.
  Case "afi_field".
    inversion H0...
  Case "afi_m_invk1".
    try (inversion H0; eauto).
  Case "afi_m_invk2".
    inversion H1; subst. 
    apply nth_error_In' in H. destruct H.
    assert (length es = length Cs) by (eauto using Forall'_len).
    edestruct Forall'_nth_error. apply H8. apply H.
    eapply IHappears_free_in.
    eapply Forall'_forall. apply H8. eauto. eauto.
  Case "afi_cast".
    inversion H0...
  Case "afi_new".
    apply nth_error_In' in H. destruct H.
    inversion H1.
    assert (length es = length Cs) by (eauto using Forall'_len).
    edestruct Forall'_nth_error. apply H6. apply H.
    eapply IHappears_free_in.
    eapply Forall'_forall. apply H6. eauto. eauto.
Qed.

Corollary typable_empty_closed : forall t C,
  empty |- t : C ->
  closed t.
Proof.
  intros.
  typing_cases (induction H using ExpTyping_ind') Case.
  Case "T_Var".
    inversion H.
  Case "T_Field".
    unfold closed in *.
  intros. intro. inversion H4. subst.
    apply IHExpTyping with x; au
    contradiction.
    constructor; auto.
Admitted.

Lemma var_subst_in: forall ds xs x i di,
  nth_error xs i = Some x ->
  nth_error ds i = Some di ->
  [; ds \ xs ;] (ExpVar x) = di.
Proof.
  intros. generalize dependent i.
  induction xs, ds; intros.
  rewrite nth_error_nil in H; inversion H.
  rewrite nth_error_nil in H; inversion H.
  rewrite nth_error_nil in H0; inversion H0.
Admitted.

(*
Lemma subst_exi_di_xi :forall ds xs e e' n,
  [; ds \ xs ;] e = e' ->
  n <= length ds ->
  exists di xi, nth_error ds n = Some di /\ nth_error xs n = Some xi.
Proof.
  intros. 
  induction H.
  exists 1.
  intro; simpl in H; inversion H.

  simpl. destruct IHsubst_list as [n'].
  exists (S n'). simpl. intro. apply H1.
  apply le_S_n; auto.
Qed.
*)
Theorem term_subst_preserv_typing : forall Gamma xs (Bs: [ClassName]) D ds As e e',
  Gamma extds xs : Bs |- e : D ->
  Forall' (ExpTyping Gamma) ds As ->
  Forall' Subtype As Bs ->
  [; ds \ xs ;] e = e' ->
  exists C, C <:D -> Gamma |- e' : C.
Proof.
  intros.
  typing_cases (induction H using ExpTyping_ind') Case.
  Case "T_Var".
    destruct (In_dec (eq_id_dec) x xs) as [xIn|xNIn].
    SCase "In x xs".
      destruct (@nth_error_In' id) with xs x as [i]; auto.
      destruct subst_nth_error with ds xs (ExpVar x) e' i x as [di]; auto.
      destruct (Forall'_nth_error _ _ (ExpTyping Gamma) ds As i di) as [Ai]; auto.
      exists Ai. intro. replace e' with di.
      eapply Forall'_forall; eauto.
      apply subst_list_det with ds xs (ExpVar x); auto. eapply var_subst_in; eauto.
    SCase "~In x xs".
      exists C; intro. 
      rewrite extend_list_not_shadow in H; auto.
      assert ( [; ds \ xs ;] (ExpVar x) = (ExpVar x)). 
      apply var_subst_notin; [ | assumption].
      apply subst_list_eq_size with (ExpVar x) (e'); auto.
      replace e' with (ExpVar x). constructor; auto.
      eapply subst_list_det; eauto.
Admitted.

Theorem subject_reduction : forall Gamma e e' C,
  Gamma |- e : C ->
  e ~> e' ->
  exists C', C' <: C -> Gamma |- e' : C'.
Proof with eauto.
  intros.
  computation_cases (induction H0) Case.
  Case "R_Field".
    subst.
    inversion H. subst.
    rename C1 into D0.
    inversion H5. subst.
    rewrite (fields_det D0 fs0 fs) in H12 by auto.
    rewrite (fields_det D0 Fs fs) in H2 by auto.
    clear H0 H8 Fs fs0.
    rename Fi into fi.
    assert (nth_error es i <> None). intro. rewrite H0 in H3. inversion H3.
    assert (List.length es = List.length Cs) by (apply (Forall'_len _ _ _ _ _ H10)).
    apply -> (nth_error_Some) in H0. rewrite H1 in H0.
    assert (exists Ci, nth_error Cs i = Some Ci). 
    apply nth_error_Some'. assumption.
    destruct H4 as [Ci].
    exists Ci.
    intro. 
    apply (Forall'_forall _ _ (ExpTyping Gamma) es Cs i ei Ci); auto.
  Case "R_Invk".
Admitted.

