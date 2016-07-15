Require Import List.
Require Import SfLib.

Notation "'[' X ']'" := (list X) (at level 40).
(* We will use Notation to make automation easier
 * This will be the notation to be similar with haskell *)

Definition ClassName := id.
Parameter Object: ClassName.

Definition Var := id.
Parameter this: Var.

Inductive Argument :=
  | Arg : id -> Argument.

Inductive Assignment :=
  | Assgnmt : id -> id -> Assignment.

Inductive FormalArg :=
  | FArg : ClassName -> id -> FormalArg.

Definition fargType (f: FormalArg):ClassName := 
  match f with FArg t _ => t end.

Inductive Constructor :=
  | KDecl : id -> [FormalArg] -> [Argument] -> [Assignment] -> Constructor.

Inductive FieldDecl :=
  | FDecl : ClassName -> id -> FieldDecl.

Inductive Exp : Set :=
  | ExpVar : Var -> Exp
  | ExpFieldAccess : Exp -> id -> Exp
  | ExpMethodInvoc: Exp -> id -> [Exp] -> Exp
  | ExpCast : ClassName -> Exp -> Exp
  | ExpNew : id -> [Exp] -> Exp.

Inductive MethodDecl :=
  | MDecl : ClassName -> id -> [FormalArg] -> id -> MethodDecl.

Inductive ClassDecl:=
  | CDecl: id -> ClassName -> [FieldDecl] -> Constructor -> [MethodDecl] -> ClassDecl.

Inductive Program :=
  | CProgram : [ClassDecl] -> Exp -> Program.

Parameter CT: @partial_map ClassDecl.
(*We will assume a global CT to make our definitions easier
 *To instance the CT use Hypothesis x: CT = ... *)

Definition isDecl (C : ClassName) := exists D fs K mds, find C CT = Some (CDecl C D fs K mds).
(*
Inductive isSuper (C D: ClassName): Prop := 
 | is_super: forall fs K mds, find C CT = Some (CDecl C D fs K mds) -> isSuper C D.
Notation "D 'is_super_of' C" := (isSuper C D) (at level 40).
*)
Hint Unfold isDecl.
Reserved Notation "C '<:' D " (at level 40).

Inductive Subtype : id -> ClassName -> Prop :=
  | S_Refl: forall C: ClassName, C <: C
  | S_Trans: forall (C D E: ClassName), 
    C <: D -> 
    D <: E -> 
    C <: E
  | S_Decl: forall C D fs K mds,
    find C CT = Some (CDecl C D fs K mds) ->
    C <: D
where "C '<:' D" := (Subtype C D).
Hint Constructors Subtype.
Tactic Notation "subtype_cases" tactic(first) ident(c) :=
  first;
  [ Case_aux c "S_Refl" | Case_aux c "S_Trans" 
  | Case_aux c "S_Decl"].

Hypothesis subtype_decls : forall C D, C <: D -> isDecl C /\ isDecl D.

(*
Lemma subtype_decls : forall C D, 
                      C <: D ->
                      isDecl C /\ isDecl D.
Proof with eauto.
  intros C D H.
  split;
  destruct H;
  unfold isDecl...
Qed.

Lemma isSuper_decls: forall C D,
  D is_super_of C -> isDecl D.
Proof.
  intros.
  destruct H.
  induction H.
  unfold isDecl.

  exists H.
  eexists.
*)


(* not sure if it is needed when assuming the declaration before subtyping
Inductive sane_ct (CT: @partial_map ClassDecl) :=
  | okDecl : forall C D fs K mds, 
            find C CT = Some (CDecl C D fs K mds) ->
            sane_ct CT
  | nobj : find Object CT = None ->
          sane_ct CT ->
  | 
            binds C (CDecl C D fs K mds) CT.
  *)
  
(* Auxiliaries *)

Definition ty := ClassName.
Definition mtyping :=  [ty] -> ty.


Inductive fields : id -> [FieldDecl] -> Prop :=
 | fields_obj : fields Object nil
 | fields_other : forall C D fs K mds fs', 
     binds C (CDecl C D fs K mds) CT ->
     fields D fs' ->
     fields C (fs'++fs).

(*Notation "ts '~>' t" := (mtyping ts t) (at level 60).*)
Print ty.

Reserved Notation "'mtype(' m ',' D ')' '=' c '~>' c0" (at level 40).
Inductive m_type (m: id) (C: ClassName) (Bs: [ClassName]) (B: ClassName) : Prop:=
  | mty_ok : forall D Fs K Ms e fargs,
              (*isDecl D ->*)
              find C CT = Some (CDecl C D Fs K Ms)->
              List.In (MDecl B m fargs e) Ms ->
              map fargType fargs = Bs ->
              mtype(m, C) = Bs ~> B
  | mty_no_override: forall D Fs K Ms e fargs,
              (*isDecl D ->*)
              find C CT = Some (CDecl C D Fs K Ms)->
              ~List.In (MDecl B m fargs e) Ms ->
              map fargType fargs = Bs ->
              mtype(m, D) = Bs ~> B ->
              mtype(m, C) = Bs ~> B
  where "'mtype(' m ',' D ')' '=' c '~>' c0"
        := (m_type m D c c0).

Hint Constructors m_type.
Tactic Notation "mty_cases" tactic(first) ident(c) :=
  first;
  [ Case_aux c "mty_ok" | Case_aux c "mty_no_override"].

Lemma mdecl_dec: forall m1 m2: MethodDecl,
  {m1=m2} + {m1 <> m2}.
Proof.
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
    inversion H0.
    destruct in_dec with (l:= mds) (a:= MDecl C0 m fargs e). exact mdecl_dec.
    eapply mty_ok...
    eapply mty_no_override...


    destruct in_dec with (l:= mds) (a:= MDecl C0 m fargs e). exact mdecl_dec.
    eapply mty_ok...
    eapply mty_no_override...
Qed.
(*

  Case "mty_ok".
    intros.
    apply subtype_decls in H1; destruct H1.
    destruct H1 as [C1 H1].
    destruct H1 as [fs H1].
    destruct H1 as [K1 H1].
    destruct H1 as [mds H1].
    destruct in_dec with (l:= mds) (a:= MDecl B m fargs e). exact mdecl_dec.
    eapply mty_ok...
    eapply mty_no_override...

  Case "mty_no_override".
    apply IHm_type.
    remember H1 as H1'. clear HeqH1'.
    apply subtype_decls in H1; destruct H1.
    apply S_Trans with C0...*)
Qed.

(*we need to assume C does not overrides the methods in D in a consistent CT*)
Admitted.

Lemma A11 : forall C D m, 
    C <: D -> 
    mtype m D = t -> 
    mtype m C = t.

Definition Bind := @partial_map Exp.

Lemma eq_var_dec : forall (v v': Var), {v = v'} + {v <> v'}.
Proof.
  intros. destruct v, v'; try eauto.
  Case "This VarId".
    right; intro.
    inversion H.
  Case "VarId This".
    right; intro; inversion H.
  Case "VarId VarId".
    destruct i, i0.
    destruct eq_nat_dec with (n:=n) (m:=n0).
    SCase "n = n0".
      left; rewrite e; reflexivity.
    SCase "n <> n0".
      right; intro H.
      inversion H. auto.
Defined.

Fixpoint subst (e: Exp) (v: Var) (v': Exp) : Exp:=
  match e with
  | ExpVar var => if eq_var_dec var v then v' else ExpVar var
  | ExpFieldAccess exp i => ExpFieldAccess (subst exp v v') i
  | ExpMethodInvoc exp i exps => 
      ExpMethodInvoc (subst exp v v') i (map (fun x => subst x v v') exps)
  | ExpCast cname exp => ExpCast cname (subst exp v v')
  | ExpNew cname exps => ExpNew cname (map (fun x => subst x v v') exps)
  end.

Notation " '[' v ':=' v' ']' e " := (subst e v v') (at level 40).  

Eval compute in ([ (VarId (Id 1)) := ExpFieldAccess (ExpVar This) (Id 2)] ExpVar (VarId (Id 1))).



