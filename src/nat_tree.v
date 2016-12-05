Require Export List.
Set Implicit Arguments.
Inductive nat_tree : Set :=
| NNode' : nat -> list nat_tree -> nat_tree.

Section All.
  Variable T : Set.
  Variable P : T -> Prop.

  Fixpoint All (ls : list T) : Prop :=
    match ls with
      | nil => True
      | cons h t => P h /\ All t
    end.
End All.

Print Forall.
Print All.

Section nat_tree_ind'.
  Variable P : nat_tree -> Prop.

  Hypothesis NNode'_case : forall (n : nat) (ls : list nat_tree),
    Forall P ls -> P (NNode' n ls).

  Fixpoint nat_tree_ind' (tr : nat_tree) : P tr :=
    match tr with
      | NNode' n ls => NNode'_case n
        ((fix list_nat_tree_ind (ls : list nat_tree) : Forall P ls :=
          match ls with
            | nil => Forall_nil P
            | cons tr' rest => Forall_cons tr' (nat_tree_ind' tr') (list_nat_tree_ind rest)
          end) ls)
    end.