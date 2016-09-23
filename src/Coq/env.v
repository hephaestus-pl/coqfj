Require Import Metatheory.
Import List.
Import ListNotations.
(*
Definition env (A:Type) := list (id * A).

Fixpoint get {A: Type} (m: env A) (x: id): option A :=
  match m with
  | nil => None
  | (x1, a) :: ms =>
    if (beq_id x x1) then Some a
    else get ms x
  end.
Locate "*".
SearchAbout prod.
 Only update our env if x is not defined yet
   This will ensure a well formed env *)
Inductive env {A: Type}: list (id * A) -> Type :=
  | env_nil : env nil
(*  | ext_single : forall x a,
    env [(x, a)]*)
  | env_ext : forall e x b,
    env e ->
    ~In x (map fst e)->
    env ((x,b):: e).
Hint Constructors env.
(*
Inductive get {A: Type} : env A -> id -> Prop:=
  | get_head : forall x b e, get ((x,b)::e) b
  .
*)

Fixpoint get {A: Type} {l: list (id * A)} (m: env l) (x: id): option A :=
  match m  with
  | env_nil => None
  | env_ext _ x1 a en _ =>
    if (beq_id x x1) then Some a
    else get en x
  end.
Notation " m 'extds' ids ':' vals" := (env (m ++ combine ids vals)) (at level 20, ids at next level).

Lemma get_if_in : forall (A: Type) (l: list (id * A)) (e: env l) x (Bi:A) e,
  @get A l e x = Some Bi ->
  In x (map fst l).
Proof.
  intros.
  induction e.
  simpl.
  unfold get in H.
  case e0.
  inversion H.
  rewrite e0 in H.
  inversion H.

Lemma exists_bs : forall (A: Type) Gamma xs (Bs: list A) (e: Gamma extds xs : Bs) x Bi,
  get e x = Some Bi ->
  exists i, nth_error Bs i = Some Bi.
Proof.
  intros.
  induction env0. inversion H.
  apply IHenv0. inversion H.
  assert (In x (map fst e)).
 subst.

Lemma env_1 : env [(Id 1, 100); (Id 2, 200); (Id 3, 300)].
Proof.
  repeat constructor; eauto.
  simpl. intro. inversion H. inversion H0. auto.
  simpl.
  intro.
  destruct H; auto. inversion H.
  destruct H; inversion H.
Qed.


Eval compute in (get env_1 (Id 1)).

Lemma env_

Lemma get_ex : get env_1 (Id 1) = Some 100.
Proof.
  unfold get.
  simpl.

Definition extend {A: Type} (m: env A) (x: id) (a: A) :=
  match (get m x) with
  | None => (x, a) :: m
  | _ => m
  end.
Notation " m 'extd' id ':' val" := (extend m id val) (at level 20, id at next level).

Inductive is_in_env {A: Type} (m: env A) (x: id) : Prop:=
  | is_in : forall a, get m x = Some a -> is_in_env m x.
Notation "x 'is_in' m" := (is_in_env m x) (at level 40). 

Fixpoint extend_list {B: Type} (m: env B) (xs: list id) (bs: list B): (env B) :=
  match xs, bs with
  | x :: xs', b :: bs' => extend_list (extend m x b) xs' bs'
  | _ , _ => m
  end.
Notation " m 'extds' ids ':' vals" := (extend_list m ids vals) (at level 20, ids at next level).

Inductive wf_env {A: Type} (m: env A) : Prop:=
  | wf1 : wf_env []
  | wf2 : forall, 

Lemma update_not_shadow: forall {A: Type} (m: env A) x a,
  x is_in m ->
  (m extd x : a) = m.
Proof.
  intros; case m in *. inversion H. inversion H0.
  inversion H.
  unfold extend.
  rewrite H0. auto.
Qed.

Theorem extend_neq : forall (X:Type) v x1 x2 (m : env X),
x2 <> x1 -> 
get (extend m x2 v) x1 = get m x1.
Proof.
intros X v x1 x2 m H.
unfold extend.
case (get m x2); auto. simpl.
rewrite not_eq_beq_id_false; auto.
Qed.

Lemma extend_list_not_shadow: forall A (m: env A) x xs bs,
  ~In x xs ->
  get (m extds xs : bs) x = get m x.
Proof.
  intros; gen xs bs.
  induction xs, bs; auto.
  apply not_in_cons in H. destruct H.
  simpl.
  unfold extend.
  destruct (get m a) eqn:Heq. apply IHxs; auto.
Admitted.
  