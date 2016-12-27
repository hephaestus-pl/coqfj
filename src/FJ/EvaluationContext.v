Require Import FJ.Base.
Require Import FJ.Syntax.
Require Import FJ.Semantics.
Require Import FJ.Axioms.
Require Import FJ.Lemmas.

(* 
  The idea of Evaluation Contexts is to represent the next reduction to be applied.
  This way we can represent easily which terms are expected to be stuck,
  which are terms with downcasts or stupidcasts as subterms.
  More details on the book Types and Programming Languages, Chapter 19 
*)

Inductive Eval_Ctx : Type :=
  | C_hole : Eval_Ctx
  | C_field_invk : Eval_Ctx -> id -> Eval_Ctx
  | C_minvk_recv: Eval_Ctx -> id -> [Exp] -> Eval_Ctx
  | C_minv_arg: Exp -> id -> [Exp] -> Eval_Ctx -> [Exp] -> Eval_Ctx
  | C_cast: ClassName -> Eval_Ctx -> Eval_Ctx
  | C_new: ClassName -> [Exp] -> Eval_Ctx -> [Exp] -> Eval_Ctx.

(* It's isCtx which actually enforces standard call-by-value 
   But since our implementation is a non deterministic reduciton, it wont be needed
   I'll leave it here anyways
*)
Inductive isCtx : Eval_Ctx -> Prop :=
  | is_hole : isCtx C_hole
  | is_field_invk : forall ev id, isCtx (C_field_invk ev id)
  | is_minvk_recv: forall ev id es, isCtx (C_minvk_recv ev id es)
  | is_minv_arg: forall v id vs es ctx, Value v -> Forall Value vs -> isCtx (C_minv_arg v id vs ctx es)
  | is_cast: forall c ctx, isCtx (C_cast c ctx)
  | is_new: forall c vs ctx es, Forall Value vs -> isCtx (C_new c vs ctx es).
Hint Constructors Eval_Ctx isCtx.

Fixpoint plug (ctx: Eval_Ctx) (e: Exp) : Exp :=
  match ctx with
  | C_hole => e
  | C_field_invk ctx' id => ExpFieldAccess (plug ctx' e) id
  | C_minvk_recv ctx' id es => ExpMethodInvoc (plug ctx' e) id es
  | C_minv_arg v id vs ctx' es => 
        ExpMethodInvoc v id (vs ++ cons (plug ctx' e) nil ++ es)
  | C_cast C ctx' => ExpCast C (plug ctx' e)
  | C_new C vs ctx' es => 
        ExpNew C (vs ++ cons (plug ctx' e) nil ++ es)
  end.
Notation "E [; t ;]" := (plug E t) (no associativity, at level 60).
Notation "[ . ]" := (C_hole) (no associativity, at level 59).


Lemma ctx_next_subterm': forall e e',
  e ~>! e' ->
  exists E r r',  e = E [; r ;] /\ 
                  e' = E [; r' ;] /\ 
                  r ~>! r'.
Proof.
  intros.
  exists C_hole e e'. split; eauto.
Qed.

Lemma ctx_next_correct: forall e e',
  e ~> e' ->
  exists E r r',  e = E [; r ;] /\ 
                  e' = E [; r' ;] /\ 
                  r ~>! r'.
Proof.
  Hint Resolve ctx_next_subterm'.
  intros.
  computation_cases (induction H) Case; eauto; destruct IHComputation as (E & r & r' & ?H & ?H & ?H).
  Case "RC_Field".
    exists (C_field_invk E f). 
    repeat eexists; crush.
  Case "RC_Invk_Recv".
    exists (C_minvk_recv E m es).
    repeat eexists; crush.
  Case "RC_Invk_Arg".
    Hint Resolve nth_error_split.
    exists (C_minv_arg e0 m (firstn i es) E (skipn (S i) es)).
    remember (skipn (S i) es).
    repeat eexists; eauto; simpl; subst;  apply f_equal.
    lets ?H: (nth_error_split es) H0; auto.
    erewrite firstn_same with (ys := es'); eauto. 
    erewrite skipn_same with (ys := es'); eauto.
  Case "RC_New_Arg".
    exists (C_new C(firstn i es) E (skipn (S i) es)).
    remember (skipn (S i) es).
    repeat eexists; eauto; simpl; subst;  apply f_equal.
    lets ?H: (nth_error_split es) H0; auto.
    erewrite firstn_same with (ys := es'); eauto. 
    erewrite skipn_same with (ys := es'); eauto.
  Case "RC_Cast".
    exists (C_cast C E). repeat eexists; crush.
Qed.
