Require Import Metatheory.
Import List.
Import ListNotations.

Definition env (A:Type) := list (id * A).
Definition wf_list {A: Type} (l: list A) :=  
  forall i1 i2 x1 x2, 
    i1 <> i2 -> 
    nth_error l i1 = Some x1 -> 
    nth_error l i2 = Some x2 -> 
    x1 <> x2.

Fixpoint get {A: Type} (m: env A) (x: id): option A :=
  match m with
  | nil => None
  | (x1, a) :: ms =>
    if (beq_id x x1) then Some a
    else get ms x
  end.


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
  intros; gen xs bs m x.
  induction xs, bs; auto; intros.
  apply not_in_cons in H. destruct H.
  simpl. rewrite IHxs; auto.
  case m. simpl. rewrite not_eq_beq_id_false; auto. intros.
  rewrite extend_neq; auto.
Qed.


Lemma ex : forall A xs bs a a0 i x bi (m: env A),
  nth_error (a :: xs) (S i) = Some x -> 
  get ((m extd a : a0) extds xs : bs) x = Some bi ->
  x <> a.
Proof.
  intros. 
  induction xs, bs; try( simpl in H; rewrite nth_error_nil in H; inversion H).
  simpl in *.
Admitted.


Lemma get_hd_ext: forall {A: Type} (m: env A) x a,
  get m x = None ->
  get (m extd x : a) x = Some a.
Proof.
  induction m; intros. simpl in *. rewrite beq_id_refl; auto.
  destruct a.
  simpl in *.
  destruct eq_id_dec with x i.
  rewrite e in H. 
rewrite beq_id_refl in H. inversion H.
  rewrite not_eq_beq_id_false in H; auto. 
Admitted.

Lemma ignore_extds : forall (A: Type) (m: env A) xs bs x,
  get m x <> None ->
  get (m extds xs : bs) x = get m x.
Admitted.

Lemma get_correct: forall A bs xs x bi i (m: env A),
  (forall x', In x' xs -> get m x' = None) ->
  wf_list xs ->
  nth_error xs i = Some x ->
  get (m extds xs : bs) x = Some bi ->
  nth_error bs i = Some bi.
Proof.
  induction bs, xs; try (intros; rewrite nth_error_nil in H1; inversion H1).  
  intros. simpl in H1.

  - rewrite H in H2. inversion H2.
    apply nth_error_In with i0; auto.


  - intros.
  case i0 in *. simpl in *.
    + inversion H1.
      rewrite ignore_extds in H2. 
      rewrite H4 in H2.
      rewrite get_hd_ext in H2. auto. auto. 
      rewrite H4; auto. rewrite get_hd_ext. intro. inversion H3. auto.

    + apply IHbs with xs x (m extd i : a) ; eauto.
      intros. 
 destruct eq_id_dec with i x.
      assert (get (m) x = None). apply H; left; auto.  
 destruct eq_id_dec with i x. rewrite e.
      rewrite get_hd_ext.
apply H. right. apply nth_error_In with i0. auto.

 destruct eq_id_dec with i x. rewrite e in H1.
  rewrite ignore_extds in H1. rewrite get_hd_ext in H1. 
  apply IHbs with xs x (m extd i : a); eauto.
  rewrite ignore_extds in H1.
  admit. 

 apply IHbs with xs x (m extd i : a); auto.
  case i in *. simpl in *. inversion H0.
  rewrite H3 in H1. admit.

  simpl in *.
  apply IHxs with x m; auto.
  simpl in H0|-*.

  rewrite <- H1. case m.
  unfold extend. simpl. admit. intros.
(*
  rewrite <- H1. simpl.
  simpl.

 unfold get in H1. simpl in H1. unfold extend in H1. simpl in H1. 
  simpl in H1. case H1.
 case m in H1.
 apply IHxs.
  admit.
  simpl. admit.

  intros; simpl in *.
  apply IHxs; auto.

 inversion H0. inversion H0.
  intros. simpl in H1.
  intros; simpl in *.
  destruct H0.
*)
Admitted.



(*
Locate "*".
SearchAbout prod.
 Only update our env if x is not defined yet
   This will ensure a well formed env 
Inductive env {A: Type}: list (id * A) -> Type :=
  | env_nil : env nil
    env [(x, a)]
  | env_ext : forall e x b,
    env e ->
    ~In x (map fst e)->
    env ((x,b):: e).
Hint Constructors env.

Inductive get {A: Type} : env A -> id -> Prop:=
  | get_head : forall x b e, get ((x,b)::e) b
  .


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
  *)