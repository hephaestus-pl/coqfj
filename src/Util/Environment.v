Require Import Referable.
Import List.
Import ListNotations.
Require Import Tactics.
Require Import Lists.
Require Import Id.

(* Add a single element to the end of the list *)
Fixpoint snoc {A: Type} (x: A) (xx: list A) : list A :=
 match xx with 
 | nil       => x :: nil
 | y :: xs'  => y :: snoc x xs'
 end.

Definition env (A:Type) := list (id * A).

Fixpoint get {A: Type} (m: env A) (x: id): option A :=
  match m with
  | nil => None
  | (x1, a) :: ms =>
    if (x =? x1) then Some a
    else get ms x
  end.

Notation "xs :> x" := (snoc x xs) (at level 62, right associativity).
Notation " m 'extd' id ':' val" := (m :> (id, val)) (at level 20, id at next level).
Notation " m 'extds' ids ':' vals" := (m ++ (combine ids vals)) (at level 20, ids at next level).

Definition Dom {A:Type} (m: env A):= map fst m.

Lemma cons_snoc_empty
 :  forall A (x: A)
 ,  x :: nil = nil :> x.
Proof. 
 auto.
Qed.
Hint Resolve cons_snoc_empty.


Lemma snoc_app
 :  forall A (xs: list A) (x: A) (ys: list A)
 ,  (ys ++ x :: xs) = ((ys :> x) ++ xs).
Proof.
  induction ys.
  * simpl in *; auto.
  * simpl in *.
    apply f_equal.
    auto.
Qed.
Hint Rewrite snoc_app.

Lemma extend_nshadow': forall {A: Type} (m: env A) x a,
  In x (Dom m) ->
  get (m extd x : a) x = get m x.
Proof.
  induction m; intros; crush.
Qed.

Lemma extend_nshadow: forall {A: Type} (m: env A) x a b,
  get m x = Some b ->
  get (m extd x : a) x = Some b.
Proof.
  induction m in *; intros; crush.  
  destruct a.
  case eqb in *; auto.
Qed.

Lemma extends_nshadow: forall {A: Type} (m: env A) x xs bs b,
  get m x = Some b ->
  get (m extds xs : bs) x = Some b.
Proof.
  induction m in *; intros; crush. 
  destruct a.
  simpl in *.
  case eqb in *; auto.
Qed.

Lemma notin_extd: forall (A: Type) (m: env A) (a: id) b (x: id),
  a <> x ->
  get (m extd a : b) x = get m x.
Proof.
  induction m; intros; simpl in *.
  * rewrite not_eq_beq_id_false; auto.
  * destruct a.
    case eqb; auto.
Qed.

Lemma notin_extds: forall (A: Type) xs bs (m: env A) x,
  ~ In x xs ->
  get (m extds xs : bs) x = get m x.
Proof.
  pose proof Decidable.not_or.
  pose proof notin_extd.
  induction xs; crush.
  destruct bs; crush.
Qed.

Lemma extends_nshadow': forall {A: Type} (m: env A) x xs bs,
  In x (Dom m) ->
  get (m extds xs : bs) x = get m x.
Proof.
  induction m in *; crush.
Qed.

Theorem extend_neq : forall (X:Type) (m : env X) v x1 x2,
    x2 <> x1 -> 
    get (m extd x2 : v) x1 = get m x1.
Proof.
  pose proof not_eq_beq_id_false.
  induction m; crush.
Qed.

Lemma get_nHead: forall (A:Type) (m: env A) x' x a,
  x' <> x ->
  get ((x', a) :: m) x = get m x.
Proof.
  pose proof not_eq_beq_id_false.
  crush.
Qed.

Lemma get_in_Dom: forall (A: Type) (b: A) m x,
  get m x = Some b ->
  In x (Dom m).
Proof.
  induction m; crush.
  destruct beq_id_dec with x a0; crush.
  rewrite not_eq_beq_id_false in H; crush.
Qed.

Lemma get_In_Dom: forall (A: Type) (m: env A) x,
  In x (Dom m) ->
  exists x', get m x = Some x'.
Proof.
  induction m; crush; eauto.
  destruct beq_id_dec with a0 x; [crush; eauto| ].
  rewrite not_eq_beq_id_false; crush.
Qed.


Lemma get_extd_m: forall (A: Type) (m: env A) x' b' x,
  In x (Dom m) ->
  get (m extd x' : b') x = get m x.
Proof.
  induction m; crush. 
Qed.

Lemma In_Dom_weak: forall (A: Type) (m: env A) x x' b,
  In x (Dom m) ->
  In x (Dom (m extd x' : b)).
Proof.
  induction m; crush.
Qed.

Lemma get_extds_m: forall (A: Type) xs bs (m: env A) x,
  In x (Dom m) ->
  get (m extds xs : bs) x = get m x.
Proof.
  pose proof get_extd_m as sth.
  pose proof In_Dom_weak as sth1.
  pose proof snoc_app as sth2.
  induction xs; crush. 
  case bs in *; crush.
Qed.

Lemma get_combine: forall (A: Type) xs (bs: list A) x i b,
  get (combine xs bs) x = Some b ->
  NoDup xs ->
  nth_error xs i = Some x ->
  nth_error bs i = Some b.
Proof.
  induction xs; crush.
  inversion H0.
  destruct i; destruct bs; crush.
  destruct beq_id_dec with x a; subst.
  apply nth_error_In in H1; crush. 
  rewrite not_eq_beq_id_false in H; auto.
  eapply IHxs; crush.
Qed.

Theorem get_noDup_extds: forall (A: Type)  xs (m: env A) (bs: list A) x b i,
  NoDup xs ->
  get (nil extds xs : bs) x = Some b ->
  nth_error xs i = Some x ->
  nth_error bs i = Some b.
Proof.
  induction 1; crush.
  eapply get_combine; eauto.
Qed.