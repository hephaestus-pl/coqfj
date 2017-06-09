Require Import String.
Require Import FJ.Lists.
Require Import FJ.Base.
(* This will be the notation to be similar with haskell *)
Notation "'[' X ']'" := (list X) (at level 40).

(* We could use Inductive for ClassNames and Vars, 
 * but would make the other definitions cumbersome to deal with 
 * the special names Object and this.
 * ClassNames are our types.
 *)
Definition ClassName := id.
Parameter Object: ClassName.

(* Vars must appear only inside methods body *)
Definition Var := id.
Parameter this: Var.

Inductive Argument :=
  | Arg : id -> Argument.

(* FormalArg and FieldDecl is a ClassName (i.e. a type) and an id *)
Inductive FormalArg :=
  | FArg : ClassName -> id -> FormalArg.
Inductive FieldDecl :=
  | FDecl : ClassName -> id -> FieldDecl.

(* The class Referable essentialy means I can use the ref function
 * to retrieve the id of a value.
 * And it also gives me a nice function ´find´ for free.
 * See Util.Referable
*)
Instance FargRef : Referable FormalArg :={
  ref farg := 
    match farg with 
   | FArg _ id => id end
}.
Instance FieldRef : Referable FieldDecl :={
  ref fdecl := 
    match fdecl with 
   | FDecl _ id => id end
}.

(* fargType and fieldType are a means to retrieve the Type of the declarations *)
Definition fargType (f: FormalArg):ClassName := 
  match f with FArg t _ => t end.
Definition fieldType (f: FieldDecl): ClassName := 
  match f with FDecl t _ => t end.

(* Our expressions are Variables,
 * field acesses,
 * method invocations,
 * cast
 * and new
*)
Inductive Exp : Type :=
  | ExpVar : Var -> Exp
  | ExpFieldAccess : Exp -> id -> Exp
  | ExpMethodInvoc : Exp -> id -> [Exp] -> Exp
  | ExpCast : ClassName -> Exp -> Exp
  | ExpNew : id -> [Exp] -> Exp.

Inductive Assignment :=
  | Assgnmt : Exp -> Exp -> Assignment.

(* Constructor declaration \texttt{C(\={C}~\={f})\{super(\={f}); this.\={f}=\={f};\}} and a constructor refinement 
 * \texttt{refines~C(\={E}~\={h}, \={C}~\={f}) \{original(\={f}); this.\={f}=\={f};\}} introduces a constructor with 
 * for the class \texttt{C} with fields \texttt{\=f} of type \texttt{\=C}. The constructor declaration body is simply 
 * a list of assignment of the arguments with its correspondent field preceded by calling its superclass constructor with the correspondent arguments.
 * The constructor refinement only differs from constructor declaration that instead of calling the superclass constructor
 * it will call its predecessor constructor (denoted by \texttt{original}).
 *)
Inductive Constructor :=
  | KDecl : id -> [FormalArg] -> [Argument] -> [Assignment] -> Constructor.


(* Method declaration \texttt{C~m~(\={C}~\={x})\ \{return~e;\}} 
 * introduces a method \texttt{m} of return type \texttt{C} with arguments \texttt{\={C}~\={x}} and body \texttt{e}.
 * Method declarations should only appear inside a class declaration.
 *)
Inductive MethodDecl :=
  | MDecl : ClassName -> id -> forall (fargs: [FormalArg]), NoDup (this :: refs fargs) -> Exp -> MethodDecl.


Instance MDeclRef : Referable MethodDecl :={
  ref mdecl := 
    match mdecl with 
   | MDecl _ id _ _ _ => id end
}.

(* A class declaration \texttt{class\ C~extends~D\ \{\={C} \={f}; K \={M}\}} 
 * introduces a class \texttt{C} with superclass \texttt{D}. This class has fields \texttt{\=f}
 * of type \texttt{C}, a constructor \texttt{K} and methdos \texttt{\=M}. The fields of class \texttt{C}
 * is \texttt{\=f} added to the fields of its superclass \texttt{D}, all of them must have distinct names.
 * Methods, in the other, hand may override another superclass method with the same name.
 * Method override in \ac{FJ} is basically method rewrite. 
 * Methods are uniquely identified by its name, i.e. overload is not supported.
 *)
Inductive ClassDecl:=
  | CDecl: id -> ClassName -> 
    forall (fDecls:[FieldDecl]), NoDup (refs fDecls) -> Constructor -> 
    forall (mDecls:[MethodDecl]), NoDup (refs mDecls) -> ClassDecl.

Instance CDeclRef : Referable ClassDecl :={
  ref cdecl := 
    match cdecl with 
   | CDecl id _ _ _ _ _ _ => id end
}.

Inductive Program :=
  | CProgram : forall (cDecls: [ClassDecl]), NoDup (refs cDecls) -> Exp -> Program.

(* We assume a fixed ClassTable *)
Parameter CT: [ClassDecl].
