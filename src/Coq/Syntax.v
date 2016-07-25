Require Export List.
Require Export SfLib.

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

Instance FargRef : Referable FormalArg :={
  ref cdecl := 
    match cdecl with 
   | FArg _ id => id end;
}.

Definition fargType (f: FormalArg):ClassName := 
  match f with FArg t _ => t end.

Inductive Constructor :=
  | KDecl : id -> [FormalArg] -> [Argument] -> [Assignment] -> Constructor.

Inductive FieldDecl :=
  | FDecl : ClassName -> id -> FieldDecl.

Inductive Exp : Set :=
  | ExpVar : Var -> Exp
  | ExpFieldAccess : Exp -> id -> Exp
  | ExpMethodInvoc : Exp -> id -> [Exp] -> Exp
  | ExpCast : ClassName -> Exp -> Exp
  | ExpNew : id -> [Exp] -> Exp.

Inductive MethodDecl :=
  | MDecl : ClassName -> id -> [FormalArg] -> Exp -> MethodDecl.

Instance MDeclRef : Referable MethodDecl :={
  ref mdecl := 
    match mdecl with 
   | MDecl _ id _ _ => id end;
}.


Inductive ClassDecl:=
  | CDecl: id -> ClassName -> [FieldDecl] -> Constructor -> [MethodDecl] -> ClassDecl.

Inductive Program :=
  | CProgram : [ClassDecl] -> Exp -> Program.

Instance CDeclRef : Referable ClassDecl :={
  ref cdecl := 
    match cdecl with 
   | CDecl id _ _ _ _ => id end;
}.

Parameter CT: [ClassDecl].
(*We will assume a global CT to make our definitions easier
 *To instance the CT use Hypothesis x: CT = ... *)

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

Inductive fields : id -> [FieldDecl] -> Prop :=
 | fields_obj : fields Object nil
 | fields_other : forall C D fs K mds fs', 
     find C CT = Some (CDecl C D fs K mds) ->
     fields D fs' ->
     fields C (fs'++fs).

Reserved Notation "'mtype(' m ',' D ')' '=' c '~>' c0" (at level 40).
Inductive m_type (m: id) (C: ClassName) (Bs: [ClassName]) (B: ClassName) : Prop:=
  | mty_ok : forall D Fs K Ms fargs e,
              find C CT = Some (CDecl C D Fs K Ms)->
              find m Ms = Some (MDecl B m fargs e) ->
              map fargType fargs = Bs ->
              mtype(m, C) = Bs ~> B
  | mty_no_override: forall D Fs K Ms,
              find C CT = Some (CDecl C D Fs K Ms)->
              find m Ms = None ->
              mtype(m, D) = Bs ~> B ->
              mtype(m, C) = Bs ~> B
  where "'mtype(' m ',' D ')' '=' c '~>' c0"
        := (m_type m D c c0).


Inductive m_body (m: id) (C: ClassName) (xs: [ClassName]) (e: Exp) : Prop:=
  | mbdy_ok : forall D Fs K Ms fargs B,
              find C CT = Some (CDecl C D Fs K Ms)->
              find m Ms = Some (MDecl B m fargs e)->
              map ref fargs = xs ->
              m_body m C xs e
  | mbdy_no_override: forall D Fs K Ms,
              find C CT = Some (CDecl C D Fs K Ms)->
              find m Ms = None->
              m_body m D xs e ->
              m_body m C xs e.
Notation "'mbody(' m ',' D ')' '=' xs 'o' e" := (m_body m D xs e) (at level 40).

Hint Constructors m_type m_body fields.
Tactic Notation "mbdy_cases" tactic(first) ident(c) :=
  first;
  [ Case_aux c "mbdy_ok" | Case_aux c "mbdy_no_override"].


Fixpoint subst (e: Exp) (v: Var) (v': Exp) : Exp:=
  match e with
  | ExpVar var => if eq_id_dec var v then v' else ExpVar var
  | ExpFieldAccess exp i => ExpFieldAccess (subst exp v v') i
  | ExpMethodInvoc exp i exps => 
      ExpMethodInvoc (subst exp v v') i (map (fun x => subst x v v') exps)
  | ExpCast cname exp => ExpCast cname (subst exp v v')
  | ExpNew cname exps => ExpNew cname (map (fun x => subst x v v') exps)
  end.
Notation " '[' v ':=' v' ']' e " := (subst e v v') (at level 40).  


