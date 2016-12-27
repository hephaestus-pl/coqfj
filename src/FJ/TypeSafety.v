Require Import FJ.Base.
Require Import FJ.Syntax.
Require Import FJ.Semantics.
Require Import FJ.Axioms.
Require Import FJ.Lemmas.
Require Import FJ.EvaluationContext.

(* This is Theorem 2.4.1 at the paper *)
Theorem preservation_step : forall Gamma e e' C,
  Gamma |-- e : C ->
  e ~>! e' ->
  exists C', C' <: C /\ Gamma |-- e' : C'.
Proof with eauto.
  intros. gen C.
  computation_step_cases (induction H0) Case; intros.
  Case "R_Field".
    subst. destruct fi; simpl in *.
    inversion H2; subst. simpl in *. destruct Fi in *. simpl in *. subst. 
    rename C1 into D0. sort. assert (C = D0). inversion H5. reflexivity. subst.
    rewrite (fields_det D0 fs Fs) in H7 by auto.
    clear H6 fs. assert ((FDecl c0 i0) = (FDecl c i0)).
    eapply ref_noDup_nth_error; eauto.  eapply fields_NoDup; eauto. inversion H3.
    inversion H5. subst. sort.
    rewrite (fields_det D0 Fs fs) in H0 by auto.
    rewrite (fields_det D0 Fs fs) in H7 by auto.
    clear H H3 H12 H7 Fs. 
    assert (nth_error es i <> None). intro. crush.
    assert (List.length es = List.length Cs) by (apply (Forall2_len _ _ _ _ _ H11)).
    apply -> (nth_error_Some) in H. rewrite H3 in H.
    assert (exists Ci, nth_error Cs i = Some Ci). 
    apply nth_error_Some'. assumption.
    destruct H4 as [Ci].
     destruct (Forall2_nth_error _ _ (Subtype) Cs (map fieldType fs) i Ci) as [fi']...
    exists Ci.
    split. sort. 
    apply map_nth_error with (B:=ClassName) (f:=fieldType) in H0; simpl in *.
    eapply Forall2_forall...
    apply (Forall2_forall _ _ (ExpTyping Gamma) es Cs i ei Ci); auto.
  Case "R_Invk".
    inversion H2. subst. inversion H6; subst; sort.
    eapply A14 in H7... 
    destruct H7 as [B]. destruct H3. destruct H3. destruct H4.
    eapply term_subst_preserv_typing with (ds := ExpNew C2 es :: ds) in H7...
    destruct H7 as [E]. destruct H7.
    exists E; split; eauto.
    apply eq_S; auto.
  Case "R_Cast". 
    assert (D = C0) by (inversion H0; crush); subst. 
    inversion_clear H0. repeat eexists; eauto. 
    assert (C = D) by (inversion H1; crush); subst.
    false. apply antisym_subtype in H2. auto.
    assert (C = D) by (inversion H1; crush); subst. contradiction.
Qed.

Theorem preservation : forall Gamma e e' C,
  Gamma |-- e : C ->
  e ~> e' ->
  exists C', C' <: C /\ Gamma |-- e' : C'.
Proof with eauto.
  intros. gen C.
  computation_cases (induction H0) Case; intros.
  Case "R_Step".
    eapply preservation_step; eauto.
  Case "RC_Field".
    inversion H; subst. eapply IHComputation in H3. 
    destruct H3 as (C' & ?H & ?H).
    lets ?H: subtype_fields H1 H4; eauto. destruct H3.
    apply nth_error_app_app with (l':=x) in H5. eauto.
  Case "RC_Invk_Recv".
    inversion H; subst. apply IHComputation in H4. 
    destruct H4 as (C' & ?H & ?H).
    eapply A11 in H1; eauto.
  Case "RC_Invk_Arg".
    inversion H4; subst.
    exists C; split; eauto.
    lets ?H: Forall2_nth_error H11 H. destruct H5 as [?C].
    lets ?H: Forall2_nth_error H12 H5. destruct H6 as [?D].
    lets: H11.
    eapply Forall2_forall with (n:=i) (x:=ei) in H11; eauto. 
    eapply IHComputation in H11. destruct H11 as (?C' & ?H & ?H).
    edestruct exists_subtyping with (es := es) (Cs := Cs) (es':= es') (Ds:= Ds) as (Cs' & ?H & ?H); eauto.
  Case "RC_New_Arg".
    inversion H4; subst.
    lets ?H: Forall2_nth_error H9 H. destruct H5 as [?C].
    lets ?H: Forall2_nth_error H11 H5. destruct H6 as [?D].
    exists C0; split; auto. 
    lets ?H: H9.
    eapply Forall2_forall with (n:=i) (x:=ei) in H8; eauto. 
    eapply IHComputation in H8. destruct H8 as (?C' & ?H & ?H).
    edestruct exists_subtyping with (es := es) (Cs := Cs) (es':= es') (Ds:= map fieldType fs) as (Cs' & ?H & ?H); eauto.
  Case "RC_Cast".
    assert (C0 = C) by (inversion H; crush); subst.
    inversion H; subst; eapply (IHComputation) in H3; destruct H3 as [C0']; destruct H1. eauto.
    rename D into C0. clear H5.
    destruct dec_subtype with C0' C.
    eapply T_UCast in H2; eauto.
    destruct dec_subtype with C C0'.
    eapply T_DCast in H2; eauto. crush.
    eapply T_SCast in H2; eauto. apply STUPID_STEP.
    rename D into C0. clear H6.
    exists C; split; eauto. eapply T_SCast; eauto. 
    eapply subtype_not_sub...
Qed.

Theorem progress: forall e C,
  nil |-- e : C ->
  normal_form Computation e ->
  Value e \/
  (exists E C D es, e = E [; ExpCast C (ExpNew D es);] /\ ~ D <: C).
Proof.
  intros.
  typing_cases (induction H using ExpTyping_ind') Case; intros.
  Case "T_Var". inversion H.
  Case "T_Field".
    Hint Rewrite fields_det.
    unfold normal_form in *. destruct IHExpTyping.
    intro. apply H0. destruct H5 as [e0']. exists (ExpFieldAccess e0' fi); eauto.
    SCase "Value".
      inversion H5; subst. inversion H; subst.
      assert (fs = fs0) by (eapply fields_det; eauto). subst. clear H1.
      false; apply H0. 
      assert (nth_error (map fieldType fs0) i = Some (fieldType Fi)) by (apply map_nth_error; eauto).
      lets ?H: Forall2_nth_error' Cs H10 H1. destruct H3 as [Ci].
      lets ?H: Forall2_nth_error' Cs H8 H3. destruct H4 as [ei].
      exists ei. constructor. econstructor; eauto.
    SCase "Stuck".
      destruct H5 as (E & C & D & es & H5 & H6).
      right. exists (C_field_invk E fi);  repeat eexists; subst; simpl ; eauto.
  Case "T_Invk".
    unfold normal_form in *. destruct IHExpTyping.
    intro. apply H0. destruct H5 as [e0']. exists (ExpMethodInvoc e0' m es); eauto.
    SCase "Value".
      inversion H5; subst. inversion H; subst. sort.
      false. apply H0. edestruct exists_mbody as (xs &e' & ?H & ?H & ?H); eauto.
      exists ([; ExpNew C0 es0 :: es \ this :: xs;] e'); constructor. constructor; eauto.
      apply Forall2_len in H2. apply Forall2_len in H3. rewrite <- H9. rewrite H2; auto.
    SCase "Stuck".
      edestruct exists_mbody as (xs &e' & ?H & ?H & ?H); eauto.
      destruct H5 as (?E & ?C & ?D & ?es & ?H & ?H). sort.
      right. exists (C_minvk_recv E m es); repeat eexists; subst; simpl; eauto.
  Case "T_New". eauto.
  Case "T_UCast".
    unfold normal_form in *. destruct IHExpTyping.
    intro. apply H0. destruct H2 as [e0']. eauto.
    SCase "Value".
      inversion H2; subst. inversion H; subst. sort.
      false. apply H0. exists (ExpNew D es); eauto. constructor; eauto. constructor. eauto.
    SCase "Stuck".
      destruct H2 as (?E & ?C & ?D & ?es & ?H & ?H). sort.
      right. exists (C_cast C E). repeat eexists; subst; simpl; eauto.
  Case "T_DCast".
    unfold normal_form in *. destruct IHExpTyping.
    intro. apply H0. destruct H3 as [e0']. eexists; eauto.
    SCase "Value".
      inversion H3; subst. inversion H; subst.
      right. exists (C_hole). simpl. repeat eexists; eauto. intro. apply antisym_subtype in H1. intuition.
    SCase "Stuck".
      destruct H3 as (?E & ?C & ?D & ?es & ?H & ?H). sort.
      right. exists (C_cast C E). repeat eexists; subst; simpl; eauto.
  Case "T_SCast".
    unfold normal_form in *. destruct IHExpTyping.
    intro. apply H0. destruct H4 as [e0']. eexists; eauto.
    SCase "Value".
      inversion H4; subst. inversion H; subst. sort.
      right. exists C_hole. simpl. eauto.
    SCase "Stuck".
      destruct H4 as (?E & ?C & ?D & ?es & ?H & ?H). sort.
      right. exists (C_cast C E); repeat eexists; subst; simpl; eauto.
Qed.
