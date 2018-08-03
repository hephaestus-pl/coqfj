Require Import Arith NPeano.
Require Import Bool.

Require Import DecidableClass.

Lemma reflectF_iff: forall P b, (reflect P b) -> (~P <-> b = false).
Proof.
  induction 1; intuition.
Qed.

Section Id.
  Inductive id : Type := 
    Id : nat -> id.

  Definition id_nat (n: id):= match n with Id x => x end.
  Definition eqb i1 i2 := Nat.eqb (id_nat i1) (id_nat i2).
  Infix "=?" := eqb.

  Lemma eqb_nat: forall (x y: nat), (Id x) = (Id y) <-> x = y.
  Proof.
    intros.
    split; intro H; try inversion H; auto.
  Qed.

  Lemma eqb_spec i1 i2 : reflect (i1 = i2) (i1 =? i2).
  Proof.
    destruct i1,i2.
    decide (n = n0); subst; simpl; unfold eqb; simpl.
    * rewrite Nat.eqb_refl.
      apply ReflectT; auto.
    * pose proof (Nat.eqb_neq n n0) as sth.
      destruct sth.
      rewrite H1; auto.
      apply ReflectF; auto.
      congruence.
  Qed.

  Instance id_eqdec : forall (i1 i2: id), Decidable (eq i1 i2) :=
    { Decidable_witness := eqb i1 i2}.
  Proof.
    pose proof (reflect_iff) as sth.
    symmetry.
    apply sth.
    apply eqb_spec.
  Qed.

  
  Theorem beq_id_refl : forall i,
      i =? i = true.
  Proof.
    intros.
    destruct i.
    apply (reflect_iff _ _ (eqb_spec _ _)).
    reflexivity.
  Qed.

  Theorem beq_id_eq : forall i1 i2,
      i1 =? i2 = true -> i1 = i2.
  Proof.
    intros i1 i2 H.
    apply (reflect_iff _ _ (eqb_spec _ _)).
    assumption.
  Qed.

  Theorem beq_id_false_neq : forall i1 i2,
      i1 =? i2 = false -> i1 <> i2.
  Proof.
    intros i1 i2 H.
    destruct i1, i2.
    apply beq_nat_false in H.
    intros C.
    inversion C; congruence.
  Qed.

  Theorem not_eq_beq_id_false : forall i1 i2,
      i1 <> i2 -> i1 =? i2 = false.
  Proof.
    intros i1 i2 H.
    destruct i1, i2.
    assert (n <> n0) by intuition.
    apply beq_nat_false_iff. assumption.
  Qed.



  Theorem beq_id_sym: forall i1 i2,
      (i1 =? i2) = (i2 =? i1).
  Proof.
    intros i1 i2.
    destruct i1, i2.
    apply Nat.eqb_sym.
  Qed.

  Theorem beq_id_dec: forall (i1 i2: id),
      {i1 = i2} + {i1 <> i2}.
  Proof. 
    repeat decide equality.
  Defined.
Section Id.

Hint Resolve beq_id_false_neq.
Hint Rewrite beq_id_refl.