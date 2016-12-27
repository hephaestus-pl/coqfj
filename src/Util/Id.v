
Require Import Arith.
Require Import NPeano.

(* Identifiers and polymorphic partial maps. *)
Inductive id : Type := 
  Id : nat -> id.

Definition beq_id id1 id2 :=
  match (id1, id2) with
    (Id n1, Id n2) => beq_nat n1 n2
  end.

Theorem beq_id_refl : forall i,
  beq_id i i = true.
Proof.
  intros. destruct i. symmetry.
  apply beq_nat_refl.  Qed.

Theorem beq_id_eq : forall i1 i2,
  beq_id i1 i2 = true -> i1 = i2.
Proof.
  intros i1 i2 H.
  destruct i1. destruct i2. symmetry in H.
  apply beq_nat_eq in H. subst.
  reflexivity.  Qed.

Theorem beq_id_false_neq : forall i1 i2,
  beq_id i1 i2 = false -> i1 <> i2.
Proof.
  intros i1 i2 H.
  destruct i1. destruct i2.
  apply beq_nat_false in H.
  intros C. apply H. inversion C. reflexivity.  Qed.

Theorem not_eq_beq_id_false : forall i1 i2,
  i1 <> i2 -> beq_id i1 i2 = false.
Proof.
  intros i1 i2 H.
  destruct i1. destruct i2.
  assert (n <> n0).
    intros C. subst. apply H. reflexivity.
  apply beq_nat_false_iff. assumption.  Qed.

Hint Resolve beq_id_false_neq.
Hint Rewrite beq_id_refl.


Theorem beq_id_sym: forall i1 i2,
  beq_id i1 i2 = beq_id i2 i1.
Proof.
  intros i1 i2. destruct i1. destruct i2. apply NPeano.Nat.eqb_sym. Qed.

Theorem beq_id_dec: forall (i1 i2: id),
  {i1 = i2} + {i1 <> i2}.
Proof. 
  repeat decide equality.
Defined.
