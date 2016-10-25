Require Export Syntax.
Require Import LibTactics.

Import Arith.

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
    (destruct in_dec with id m (map ref mds);
      [ exact eq_id_dec
      | eapply mty_ok; eauto 
      | eapply mty_no_override; eauto
    ]).
Qed.


Lemma weakening: forall Gamma e x C D,
  Gamma |- e : C ->
  get Gamma x = None ->
  Gamma extd x : D |- e : C.
Proof with eauto.
  intros.
  typing_cases (induction H using ExpTyping_ind') Case; try (solve [econstructor; eauto]).
  Case "T_Var".
    constructor.
    destruct eq_id_dec with x x0; subst. rewrite H0 in H. inversion H.
    rewrite extend_neq; auto.
Qed.

Lemma A14: forall D D0 m C0 xs Ds e,
  mtype(m,C0) = Ds ~> D ->
  mbody(m,C0) = xs o e ->
  C0 <: D0 -> 
  exists C, C <: D /\
  nil extds (this :: xs) : (C0 :: Ds) |- e : C.
Proof.
  intros.
  mbdy_cases (induction H0) Case.
  Case "mbdy_ok".
(*
    Print T_Method.*)
Admitted.

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

Lemma fields_NoDup : forall C fs,
  fields C fs ->
  NoDup (refs fs).
Proof.
  intros.
  inversion H; [simpl; constructor | assumption].
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
    inversion H4.
    subst. sort.
    rewrite sane_CT in H. inversion H.
    subst.
    rewrite H in H5.
    inversion H5. subst.
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
  wf_extd Gamma xs ->
  Gamma extds xs : Bs |- e : D ->
  Forall2 (ExpTyping Gamma) ds As ->
  Forall2 Subtype As Bs ->
  length ds = length xs ->
  exists C, (C <:D /\ Gamma |- [; ds \ xs ;] e : C).
Proof with eauto.
  intros.
  typing_cases (induction H0 using ExpTyping_ind') Case.
  Case "T_Var".
    destruct (In_dec (eq_id_dec) x xs) as [xIn|xNIn].
    SCase "In x xs". rename C into Bi. SearchAbout Forall2.
      assert (In x xs); eauto.
      apply nth_error_In' in xIn as [i]. symmetry in H3.
      edestruct (@nth_error_same_len id Exp) as [di]...
      assert (nth_error Bs i = Some Bi).
      eapply get_wf_extds; eauto. 
      erewrite var_subst_in...
      destruct (Forall2_nth_error _ _ (ExpTyping Gamma) ds As i di) as [Ai]...
      exists Ai.
      split.
      eapply Forall2_forall...
      eapply Forall2_forall...
    SCase "~In x xs".
      split with C. split. eauto.
      erewrite notin_extds in H0...
      assert ( [; ds \ xs ;] (ExpVar x) = (ExpVar x)). simpl. rewrite notin_findwhere; auto. rewrite H4. 
      constructor...
  Case "T_Field".
      
Admitted.

Lemma ref_noDup_nth_error: forall {T} {H: Referable T} (xs:list T) i i1 x x1,
  nth_error xs i = Some x ->
  nth_error xs i1 = Some x1 ->
  NoDup (refs xs) ->
  ref x = ref x1 ->
  x = x1.
Proof.
Admitted.

Theorem subject_reduction : forall Gamma e e' C,
  Gamma |- e : C ->
  e ~> e' ->
  exists C', C' <: C /\ Gamma |- e' : C'.
Proof with eauto.
  intros.
  computation_cases (induction H0) Case.
  Case "R_Field".
    subst. destruct fi; simpl in *. 
    inversion H. subst. simpl in *. destruct Fi in *. simpl in *. subst. 
    rename C1 into D0. sort. assert (C0 = D0). inversion H5. reflexivity. subst.
    rewrite (fields_det D0 fs Fs) in H7 by auto.
    clear H6 fs. assert ((FDecl c0 i0) = (FDecl c i0)).
    eapply ref_noDup_nth_error; eauto.  eapply fields_NoDup; eauto. inversion H3.
    inversion H5. subst. sort.
    rewrite (fields_det D0 Fs fs) in H1 by auto.
    rewrite (fields_det D0 Fs fs) in H7 by auto.
    clear H0 H12 H7 Fs. 
    assert (nth_error es i <> None). intro. rewrite H0 in H2. inversion H2.
    assert (List.length es = List.length Cs) by (apply (Forall2_len _ _ _ _ _ H11)).
    apply -> (nth_error_Some) in H0. rewrite H4 in H0.
    assert (exists Ci, nth_error Cs i = Some Ci). 
    apply nth_error_Some'. assumption.
    destruct H6 as [Ci].
     destruct (Forall2_nth_error _ _ (Subtype) Cs (map fieldType fs) i Ci) as [fi']...
    exists Ci.
    split. sort.
    apply map_nth_error with (B:=ClassName) (f:=fieldType) in H1; simpl in *.
    eapply Forall2_forall...
    apply (Forall2_forall _ _ (ExpTyping Gamma) es Cs i ei Ci); auto.
  Case "R_Invk".
    inversion H. subst. inversion H4; subst. inversion H; subst. sort.
    eapply A14 in H5. destruct H5. destruct H1.
    eapply term_subst_preserv_typing in H2. destruct H2. destruct H2.
Admitted.

