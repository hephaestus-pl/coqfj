Require Export Syntax.
Require Import LibTactics.
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


Lemma var_subst_in: forall ds xs x i di,
  nth_error xs i = Some x ->
  nth_error ds i = Some di ->
  [; ds \ xs ;] (ExpVar x) = di.
Proof.
  intros. gen ds xs i.
  induction ds, xs; intros.
  rewrite nth_error_nil in H; inversion H.
  rewrite nth_error_nil in H0; inversion H0.
  rewrite nth_error_nil in H; inversion H.
  apply findwhere_ntherror in H. unfold subst.
  rewrite H; simpl. rewrite H0. auto.
Qed.

Theorem term_subst_preserv_typing : forall Gamma xs (Bs: [ClassName]) D ds As e,
  Gamma extds xs : Bs |- e : D ->
  Forall2 (ExpTyping Gamma) ds As ->
  Forall2 Subtype As Bs ->
  length ds = length xs ->
  exists C, C <:D -> Gamma |- [; ds \ xs ;] e : C.
Proof with eauto.
  intros.
  typing_cases (induction H using ExpTyping_ind') Case.
  Case "T_Var".
    destruct (In_dec (eq_id_dec) x xs) as [xIn|xNIn].
    SCase "In x xs".
      apply nth_error_In' in xIn as [i]. symmetry in H2.
      edestruct (@nth_error_same_len id Exp) as [di]...
      erewrite var_subst_in...
      destruct (Forall2_nth_error _ _ (ExpTyping Gamma) ds As i di) as [Ai]...
      exists Ai. intro. eapply Forall2_forall...
    SCase "~In x xs".
      exists C; intro. 
      rewrite extend_list_not_shadow in H...
      assert ( [; ds \ xs ;] (ExpVar x) = (ExpVar x)). simpl. rewrite notin_findwhere; auto. rewrite H4. 
      constructor...
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
    assert (List.length es = List.length Cs) by (apply (Forall2_len _ _ _ _ _ H10)).
    apply -> (nth_error_Some) in H0. rewrite H1 in H0.
    assert (exists Ci, nth_error Cs i = Some Ci). 
    apply nth_error_Some'. assumption.
    destruct H4 as [Ci].
    exists Ci.
    intro. 
    apply (Forall2_forall _ _ (ExpTyping Gamma) es Cs i ei Ci); auto.
  Case "R_Invk".
Admitted.

