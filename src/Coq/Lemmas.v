Require Export Syntax.

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
    intros.
    inversion H0; (destruct in_dec with (l:= mds) (a:= MDecl C0 m fargs e);
      [ exact mdecl_dec 
      | eapply mty_ok; eauto 
      | eapply mty_no_override; eauto
      ]).
Qed.


Eval compute in ([ (Id 1) := ExpFieldAccess (ExpVar this) (Id 2)] ExpVar (Id 1)).
