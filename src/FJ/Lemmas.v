Require Import Relation_Definitions.
Require Import Tactics.
Require Import Lists.
Require Import Base.
Require Import Syntax.

(* Auxiliary Lemmas *)
(* mtype / MType_OK lemmas *)
Lemma unify_returnType : forall Ds D C D0 Fs noDupfs K Ms noDupMds C0 m fargs noDupfargs ret,
  mtype( m, C)= Ds ~> D ->
  find C CT = Some (CDecl C D0 Fs noDupfs K Ms noDupMds) ->
  find m Ms = Some (MDecl C0 m fargs noDupfargs ret) ->
  D = C0.
Proof.
  induction 1; crush.
Qed.

Lemma unify_fargsType : forall Ds D C D0 Fs noDupfs K Ms noDupMds C0 m fargs noDupfargs ret,
  mtype( m, C)= Ds ~> D ->
  find C CT = Some (CDecl C D0 Fs noDupfs K Ms noDupMds) ->
  find m Ms = Some (MDecl C0 m fargs noDupfargs ret) ->
  Ds = map fargType fargs.
Proof.
  induction 1; crush.
Qed.

Lemma methodDecl_OK :forall C D0 Fs noDupfs K Ms noDupMds C0 m fargs noDupfargs ret,
  find m Ms = Some (MDecl C0 m fargs noDupfargs ret) ->
  find C CT = Some (CDecl C D0 Fs noDupfs K Ms noDupMds) ->
  CType_OK (CDecl C D0 Fs noDupfs K Ms noDupMds) ->
  MType_OK C (MDecl C0 m fargs noDupfargs ret).
Proof.
  inversion 3. 
  match goal with
  [ H: Forall _ _ |- _ ] =>  eapply Forall_find in H; eauto
  end.
Qed.

(* fields Lemmas *)
Lemma fields_obj_nil: forall f,
  fields Object f -> f = nil.
Proof.
  remember Object.
  induction 1; crush.
Qed.

Lemma fields_NoDup : forall C fs,
  fields C fs ->
  NoDup (refs fs).
Proof.
  inversion 1; crush.
Qed.

Lemma fields_det: forall C f1 f2,
  fields C f1 ->
  fields C f2 ->
  f1 = f2.
Proof.
  Hint Resolve fields_obj_nil.
  intros.
  gen f1.
  fields_cases (induction H0) Case; intros.
  Case "F_Obj".
    crush.
  Case "F_Decl".
    match goal with 
    [ H: fields _ _ |- _ ] => destruct H; [crush |]
    end.
    match goal with
    [ H: fields _ ?fs |- _] => specialize IHfields with fs; crush
    end.
Qed.

(* Subtype Lemmas *)

Lemma obj_not_subtype: forall C,
  C <> Object -> ~ Object <: C.
Proof.
  intros; intro. 
  remember Object. induction H0; [auto | | crush].
  subst. destruct beq_id_dec with D Object; subst; auto.
Qed.


Lemma subtype_fields: forall C D fs ,
  C <: D ->
  fields D fs ->
  exists fs', fields C (fs ++ fs').
Proof.
  Hint Rewrite app_nil_r app_assoc.
  intros. gen H0. gen fs.
  subtype_cases (induction H) Case; intros.
  Case "S_Refl".
    exists (@nil FieldDecl); crush.
  Case "S_Trans".
    edestruct IHSubtype2; eauto.
    edestruct IHSubtype1; eauto.
    eexists ; crush; eassumption.
  Case "S_Decl".
    exists (fs); auto. eapply F_Decl; eauto.
    apply ClassesOK in H. inversion H; subst; auto.
    assert (fs0 = fdecl) by (apply fields_det with D; auto).
    crush.
Qed.

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
  induction H; [crush | intros | crush].
  destruct beq_id_dec with C D.
  eapply IHSubtype2; crush.
  edestruct IHSubtype1; eauto.
Qed.

Lemma subtype_not_sub': forall C D E,
  E <: C ->
  E <: D ->
  C <: D \/ D <: C.
Proof.
  intros C D E H. gen D.
  induction H; auto; intros.
  edestruct IHSubtype1; eauto.
  destruct beq_id_dec with C D0. subst. apply S_Decl in H. auto.
  eapply super_class_subtype in H0; eauto.
Qed.

Lemma subtype_not_sub: forall C D E,
    E <: D ->
  ~ C <: D ->
  ~ D <: C ->
  ~ E <: C.
Proof.
  intros C D E H H0 H1 H2.
  apply subtype_not_sub' with (D:=D) in H2; eauto.
  destruct H2; auto.
Qed.

(* subst Lemmas *)
Lemma var_subst_in: forall ds xs x i di,
  nth_error xs i = Some x ->
  nth_error ds i = Some di ->
  NoDup xs ->
  [; ds \ xs ;] (ExpVar x) = di.
Proof.
  Hint Rewrite nth_error_nil.
  intros. gen ds xs i.
  induction ds, xs; crush.
  apply findwhere_ntherror in H; crush.
Qed.

(* Paper Lemmas *)

Lemma super_obj_or_defined: forall C D Fs noDupfs K Ms noDupMds,
    find C CT = Some (CDecl C D Fs noDupfs K Ms noDupMds) ->
    D = Object \/ exists D0 Fs0 noDupfs0 K0 Ms0 noDupMds0, 
                    find D CT = Some (CDecl D D0 Fs0 noDupfs0 K0 Ms0 noDupMds0).
Proof.
  intros. destruct beq_id_dec with D Object; subst. 
  left; auto.
  right. eapply superClass_in_dom; eauto.
Qed.

Lemma mtype_obj_False: forall m Cs C,
  mtype(m, Object) = Cs ~> C ->
  False.
Proof.
  inversion 1; crush.
Qed.

Lemma unify_find_mname: forall m Ms c i fargs n e,
  find m Ms = Some (MDecl c i fargs n e) ->
  find m Ms = Some (MDecl c m fargs n e) /\ m = i.
Proof.
  intros.
  assert (ref (MDecl c i fargs n e) = m). 
  eapply find_ref_inv; eauto. crush.
Qed.

Lemma methods_same_signature: forall C D Fs noDupfs K Ms noDupMds Ds D0 m,
    find C CT = Some (CDecl C D Fs noDupfs K Ms noDupMds) ->
    mtype(m, D) = Ds ~> D0 ->
    mtype(m, C) = Ds ~> D0.
Proof.
  Hint Resolve mtype_obj_False.  
  intros. apply ClassesOK in H.
  inversion H; subst; sort; clear H.
  edestruct super_obj_or_defined; eauto; subst.
  false; eapply mtype_obj_False; eauto.
  destruct H as (?D & Fs1 & noDupfs0 & K0 & Ms0 & noDupMds0 & H).
  destruct (@find_dec MethodDecl) with MDeclRef Ms m. destruct e. destruct x. sort.
  apply unify_find_mname in H1; destruct H1; subst.
  eapply Forall_find in H9; [|eexact H1].
  destruct H9; subst; sort. assert (D2 = D) by crush; subst.
  apply unify_find_mname in H1. destruct H1; subst.
  destruct H5 with Ds D0; subst; auto. eapply mty_ok; crush.
  eapply mty_no_override; eauto.
Qed.


Lemma A11: forall m D C Cs C0,
          C <: D ->
          mtype(m,D) = Cs ~> C0 ->
          mtype(m,C) = Cs ~> C0.
Proof.
  Hint Resolve methods_same_signature.
  induction 1; eauto.
Qed.


Lemma weakening: forall Gamma e C,
  nil |-- e : C ->
  Gamma |-- e : C.
Proof.
  induction 1 using ExpTyping_ind'; eauto.
  crush.
Qed.

Lemma A14: forall D m C0 xs Ds e,
  mtype(m,C0) = Ds ~> D ->
  mbody(m,C0) = xs o e ->
  exists D0 C,  C0 <: D0 /\ C <: D /\
  nil extds (this :: xs) : (D0 :: Ds) |-- e : C.
Proof.
  intros.
  mbdy_cases (induction H0) Case.
  Case "mbdy_ok". 
    lets: H1.
    eapply methodDecl_OK with (C:=C) in H1; eauto. 
    inversion H1; clear H1; sort; subst.
    exists C E0. split; auto.  clear H11.
    erewrite unify_returnType with (D := D) (C0 := C0); eauto.
    split. assumption.
    erewrite unify_fargsType with (Ds := Ds) (fargs := fargs); eauto.

  Case "mbdy_no_override".
    inversion H; [crush|]. 
    assert (D1 = D0) by crush; subst.
    eapply IHm_body in H5.
    destruct H5 as (C1 & E0 & ?H & ?H & ?H).
    exists C1 E0; eauto.
Qed.


Theorem term_subst_preserv_typing : forall Gamma xs (Bs: list ClassName) D ds As e,
  nil extds xs : Bs |-- e : D ->
  NoDup xs ->
  Forall2 (ExpTyping Gamma) ds As ->
  Forall2 Subtype As Bs ->
  length ds = length xs ->
  exists C, (C <:D /\ Gamma |-- [; ds \ xs ;] e : C).
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

(* This is Theorem 2.4.1 at the paper *)
Theorem subject_reduction : forall Gamma e e' C,
  Gamma |-- e : C ->
  e ~> e' ->
  exists C', C' <: C /\ Gamma |-- e' : C'.
Proof with eauto.
  intros. gen C.
  computation_cases (induction H0) Case; intros.
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
  Case "RC_Field".
    inversion H; subst. eapply IHComputation in H3. 
    destruct H3 as (C' & ?H & ?H).
    admit.
  Case "RC_Invk_Recv".
    inversion H; subst. apply IHComputation in H4. 
    destruct H4 as (C' & ?H & ?H).
    eapply A11 in H1; eauto.
  Case "RC_Invk_Arg".
    inversion H3; subst.    
    exists C; split; eauto. econstructor; eauto.
    lets ?H: Forall2_nth_error H10 H. destruct H4 as [?C].
    lets ?H: Forall2_nth_error H11 H4. destruct H5 as [?D].
    admit.
apply IHComputation in H6.
    admit.
  Case "RC_New_Arg".
    admit.
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
Admitted.

