Require Import Relation_Definitions.
Require Import Tactics.
Require Import Lists.
Require Import Base.
Require Import Syntax.

Import Arith.

(* Auxiliary Lemmas *)
(* mtype / MType_OK lemmas *)
Lemma unify_returnType : forall Ds D C D0 Fs noDupfs K Ms noDupMds C0 m fargs noDupfargs ret,
  mtype( m, C)= Ds ~> D ->
  find C CT = Some (CDecl C D0 Fs noDupfs K Ms noDupMds) ->
  find m Ms = Some (MDecl C0 m fargs noDupfargs ret) ->
  D = C0.
Proof.
  intros; induction H; crush.
Qed.

Lemma unify_fargsType : forall Ds D C D0 Fs noDupfs K Ms noDupMds C0 m fargs noDupfargs ret,
  mtype( m, C)= Ds ~> D ->
  find C CT = Some (CDecl C D0 Fs noDupfs K Ms noDupMds) ->
  find m Ms = Some (MDecl C0 m fargs noDupfargs ret) ->
  Ds = map fargType fargs.
Proof.
  intros; induction H; crush.
Qed.

Lemma methodDecl_OK :forall C D0 Fs noDupfs K Ms noDupMds C0 m fargs noDupfargs ret,
  find m Ms = Some (MDecl C0 m fargs noDupfargs ret) ->
  find C CT = Some (CDecl C D0 Fs noDupfs K Ms noDupMds) ->
  CType_OK (CDecl C D0 Fs noDupfs K Ms noDupMds) ->
  MType_OK C (MDecl C0 m fargs noDupfargs ret).
Proof.
  intros. inversion H1. eapply Forall_find in H9; eauto.
Qed.

(* fields Lemmas *)
Lemma fields_obj_nil: forall f,
  fields Object f -> f = nil.
Proof.
  intros.
  remember Object.
  induction H; auto.
  rewrite Heqc in H.
  rewrite obj_notin_dom in H.
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
    rewrite obj_notin_dom in H. inversion H.
    subst.
    rewrite H in H5.
    inversion H5. subst.
    rewrite IHfields with fs'0; auto.
Qed.

(* Subtype Lemmas *)
Lemma subtype_fields: forall C D fs ,
  C <: D ->
  fields D fs ->
  exists fs', fields C (fs ++ fs').
Proof.
Admitted.

Lemma subtype_order:
  order _ Subtype.
Proof.
  refine {| ord_refl:= (S_Refl); ord_trans:= (S_Trans); ord_antisym:=antisym_subtype|}.
Qed.

Lemma super_class_subtype: forall C D D0 fs noDupfs K mds noDupMds,
 C <: D -> C <> D ->
 find C CT = Some (CDecl C D0 fs noDupfs K mds noDupMds) ->
 D0 <: D.
Proof.
  intros C D D0 fs noDupfs K mds noDupMds H.
  gen D0 fs noDupfs K mds noDupMds.
  induction H; intros; auto. false; apply H; auto. sort.
  destruct beq_id_dec with C D. rewrite e in H2; auto. eapply IHSubtype2; eauto. rewrite <- e; auto.
  edestruct IHSubtype1; eauto.
  rewrite H1 in H; crush.
Qed.

Lemma subtype_not_sub': forall C D E,
  E <: C ->
  E <: D ->
  C <: D \/ D <: C.
Proof.
  intros C D E H. gen D. induction H; auto.
  intros.
  edestruct IHSubtype1; eauto.
  intros. destruct beq_id_dec with C D0. subst. apply S_Decl in H. right; auto.
  eapply super_class_subtype in H0; eauto.
Qed.

Lemma subtype_not_sub: forall C D E,
    E <: D ->
  ~ C <: D ->
  ~ D <: C ->
  ~ E <: C.
Proof.
  intros.
  intro. apply subtype_not_sub' with (D:=D) in H2; eauto. destruct H2; auto.
Qed.

(* subst Lemmas *)
Lemma var_subst_in: forall ds xs x i di,
  nth_error xs i = Some x ->
  nth_error ds i = Some di ->
  NoDup xs ->
  [; ds \ xs ;] (ExpVar x) = di.
Proof.
  intros. gen ds xs i.
  induction ds, xs; intros.
  rewrite nth_error_nil in H; inversion H.
  rewrite nth_error_nil in H0; inversion H0.
  rewrite nth_error_nil in H; inversion H.
  apply findwhere_ntherror in H; auto. unfold subst.
  rewrite H; simpl. rewrite H0. auto.
Qed.

(* Paper Lemmas *)

Lemma super_obj_or_defined: forall C D Fs noDupfs K Ms noDupMds,
    find C CT = Some (CDecl C D Fs noDupfs K Ms noDupMds) ->
    D = Object \/ exists D0 Fs0 noDupfs0 K0 Ms0 noDupMds0, find D CT = Some (CDecl D D0 Fs0 noDupfs0 K0 Ms0 noDupMds0).
Proof.
  intros. destruct beq_id_dec with D Object; subst. left; auto.
  right. eapply superClass_in_dom; eauto.
Qed.

Lemma mtype_obj_False: forall m Cs C,
  mtype(m, Object) = Cs ~> C ->
  False.
Proof.
  Hint Rewrite obj_notin_dom. intros.
  inversion H; crush.
Qed.

Lemma methods_same_signature: forall C D Fs noDupfs K Ms noDupMds Ds D0 m,
    find C CT = Some (CDecl C D Fs noDupfs K Ms noDupMds) ->
    mtype(m, D) = Ds ~> D0 ->
    mtype(m, C) = Ds ~> D0.
Proof.
  intros. apply ClassesOK in H. inversion H. subst; sort. clear H.
  edestruct super_obj_or_defined; eauto. subst.
  false. eapply mtype_obj_False; eauto.
  destruct H as (?D & Fs1 & noDupfs0 & K0 & Ms0 & noDupMds0 & H).
Admitted.


Lemma A11: forall m D C Cs C0,
          C <: D ->
          mtype(m,D) = Cs ~> C0 ->
          mtype(m,C) = Cs ~> C0.
Proof with eauto.
  intros m D C Cs C0 H.
  subtype_cases (induction H) Case...
  Case "S_Decl".
    intros.
    inversion H0; sort.
    destruct (@find_dec MethodDecl) with MDeclRef mds m.
    destruct e0. destruct x. eapply mty_ok. eexact H. 
    assert (i = m).
    change (ref (MDecl c i fargs0 n e0) = m). 
    apply find_ref_inv with (d:=mds); eauto.
    rewrite H5 in H4. clear H5. (*
    assert (C0 = c). eapply subtype_method_same_type; eauto. crush.
    rewrite <- H3. symmetry. eapply subtype_method_same_type'; eauto.
    
    assert (i = m). 
    assert (i = ref (MDecl c i fargs0 n e0)); auto. rewrite H5.
    apply find_ref_inv with (d:=mds); auto.
    crush.
    eapply mty_no_override; eauto. *)
    
    
    
Admitted.


Lemma weakening: forall Gamma e C,
  nil |- e : C ->
  Gamma |- e : C.
Proof with eauto.
  intros.
  typing_cases (induction H using ExpTyping_ind') Case;  try (solve [econstructor; eauto]).
  Case "T_Var".
    inversion H; eauto.
Qed.

Lemma A14: forall D m C0 xs Ds e,
  mtype(m,C0) = Ds ~> D ->
  mbody(m,C0) = xs o e ->
  exists D0 C,  C0 <: D0 /\ C <: D /\
  nil extds (this :: xs) : (D0 :: Ds) |- e : C.
Proof.
  intros.
  mbdy_cases (induction H0) Case.
  Case "mbdy_ok".
    sort.
    lets: H1.
    eapply methodDecl_OK with (C:=C) in H1; eauto. 
    inversion H1. clear H1; sort. subst.
    exists C E0. split; auto.  clear H11.
    erewrite unify_returnType with (D := D) (C0 := C0); eauto.
    split. assumption.
    erewrite unify_fargsType with (Ds := Ds) (fargs := fargs); eauto.

  Case "mbdy_no_override".
    inversion H. subst. sort. rewrite H3 in H0; inversion H0; subst. 
    rewrite H4 in H1; inversion H1. rewrite H3 in H0; inversion H0; subst; clear H0.
    eapply IHm_body in H5. 
    destruct H5 as [C1]. destruct H0 as [E0]. destruct H0. destruct H5.
    exists C1 E0. split; eauto; split; eauto.
Qed.


Theorem term_subst_preserv_typing : forall Gamma xs (Bs: [ClassName]) D ds As e,
  nil extds xs : Bs |- e : D ->
  NoDup xs ->
  Forall2 (ExpTyping Gamma) ds As ->
  Forall2 Subtype As Bs ->
  length ds = length xs ->
  exists C, (C <:D /\ Gamma |- [; ds \ xs ;] e : C).
Proof with eauto.
  intros.
  typing_cases (induction H using ExpTyping_ind') Case; sort.
  Case "T_Var".
    destruct (In_dec (beq_id_dec) x xs) as [xIn|xNIn].
    SCase "In x xs". rename C into Bi.
      assert (In x xs); eauto.
      apply nth_error_In' in xIn as [i]. symmetry in H3.
      edestruct (@nth_error_same_len id Exp) as [di]...
      assert (nth_error Bs i = Some Bi).
      eapply get_wf_extds; eauto; constructor; eauto. 
      destruct (Forall2_nth_error _ _ (ExpTyping Gamma) ds As i di) as [Ai]...
      exists Ai.
      split.
      eapply Forall2_forall... erewrite var_subst_in; eauto.
      eapply Forall2_forall...
    SCase "~In x xs". 
      split with C. split. eauto.
      erewrite notin_extds in H... inversion H. 
  Case "T_Field".
    simpl. destruct IHExpTyping as [C']. destruct H8. 
    exists Ci. 
    split...
    eapply subtype_fields in H8... destruct H8 as [fs'].
    eapply T_Field. eassumption.  eapply H8. eapply nth_error_app_app... auto. auto.
  Case "T_Invk". rename C0 into D0.
    destruct IHExpTyping as [C0]. destruct H8.
    apply A11 with (m:=m) (Cs:=Ds) (C0:=C) in H8...
    exists C. split; auto. simpl. 
    apply Forall2_exi in H7. destruct H7 as [Cs']. sort. destruct H7.
    apply Forall2_trans with (zs:= Ds) in H7; auto.
    eapply T_Invk; eauto.
    apply Forall2_map; auto.
    intros x y z ?H ?H1; apply S_Trans with y; auto. 
  Case "T_New".
    apply Forall2_exi in H7. destruct H7 as [Cs']. destruct H7; sort.
    exists C; split; auto. simpl. 
    apply Forall2_trans with (zs:= Ds) in H7; auto.
    eapply T_New...
    apply Forall2_map; auto.
    intros x y z ?H ?H1; apply S_Trans with y; auto.
  Case "T_UCast".
    exists C. split; auto. simpl.
    destruct IHExpTyping as [E]. destruct H5.
    eapply T_UCast...
  Case "T_DCast".
    exists C; split; auto. simpl.
    destruct IHExpTyping as [E]. destruct H6.
    destruct dec_subtype with E C.
    eapply T_UCast in H7...
    destruct beq_id_dec with E C. rewrite e in H8; false; apply H8; auto.
    destruct dec_subtype with C E.
    eapply T_DCast in H7...
    eapply T_SCast in H7...
    apply STUPID_STEP.
  Case "T_SCast".
    exists C; split; auto. simpl.
    destruct IHExpTyping as [E]. destruct H7.
    eapply T_SCast...
    eapply subtype_not_sub...
Qed. 


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
    inversion H. subst. inversion H6; subst; sort.
    rename C2 into C0.
    eapply A14 in H7... 
    destruct H7 as [B]. destruct H3. destruct H3. destruct H4.
    eapply term_subst_preserv_typing with (ds := ExpNew C0 es :: ds) in H7...
    destruct H7 as [E]. destruct H7.
    exists E; split; eauto.
    apply eq_S; auto.
  Case "R_Cast".
    
Admitted.

