-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module FJ.Syntax.Parfj_syntax where
import FJ.Syntax.Absfj_syntax
import FJ.Syntax.Lexfj_syntax
import FJ.Syntax.ErrM

}

%name pProgram Program
%name pClassDecl ClassDecl
%name pFieldDecl FieldDecl
%name pConstructor Constructor
%name pField Field
%name pFormalArg FormalArg
%name pArg Arg
%name pAssignment Assignment
%name pMethodDecl MethodDecl
%name pTerm Term
%name pExp Exp
%name pType Type
%name pListClassDecl ListClassDecl
%name pListFieldDecl ListFieldDecl
%name pListMethodDecl ListMethodDecl
%name pListField ListField
%name pListFormalArg ListFormalArg
%name pListArg ListArg
%name pListAssignment ListAssignment
%name pListTerm ListTerm

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype { Token }

%token 
 '(' { PT _ (TS _ 1) }
 ')' { PT _ (TS _ 2) }
 ',' { PT _ (TS _ 3) }
 '.' { PT _ (TS _ 4) }
 ';' { PT _ (TS _ 5) }
 '=' { PT _ (TS _ 6) }
 'Object' { PT _ (TS _ 7) }
 'class' { PT _ (TS _ 8) }
 'extends' { PT _ (TS _ 9) }
 'new' { PT _ (TS _ 10) }
 'return' { PT _ (TS _ 11) }
 'super' { PT _ (TS _ 12) }
 'this' { PT _ (TS _ 13) }
 '{' { PT _ (TS _ 14) }
 '}' { PT _ (TS _ 15) }

L_Id { PT _ (T_Id $$) }
L_err    { _ }


%%

Id    :: { Id} : L_Id { Id ($1)}

Program :: { Program }
Program : ListClassDecl Exp { CProgram (reverse $1) $2 } 


ClassDecl :: { ClassDecl }
ClassDecl : 'class' Id 'extends' Type '{' ListFieldDecl Constructor ListMethodDecl '}' { CDecl $2 $4 (reverse $6) $7 (reverse $8) } 


FieldDecl :: { FieldDecl }
FieldDecl : Type Id ';' { FDecl $1 $2 } 


Constructor :: { Constructor }
Constructor : Id '(' ListField ')' '{' 'super' '(' ListArg ')' ';' ListAssignment '}' { KDecl $1 $3 $8 (reverse $11) } 


Field :: { Field }
Field : Type Id { Field $1 $2 } 


FormalArg :: { FormalArg }
FormalArg : Type Id { FormalArg $1 $2 } 


Arg :: { Arg }
Arg : Id { Arg $1 } 


Assignment :: { Assignment }
Assignment : 'this' '.' Id '=' Id ';' { Assignment $3 $5 } 


MethodDecl :: { MethodDecl }
MethodDecl : Type Id '(' ListFormalArg ')' '{' 'return' Term ';' '}' { MethodDecl $1 $2 $4 $8 } 


Term :: { Term }
Term : Id { TermVar $1 } 
  | Term '.' Id { TermFieldAccess $1 $3 }
  | Term '.' Id '(' ListTerm ')' { TermMethodInvoc $1 $3 $5 }
  | Exp { TermExp $1 }


Exp :: { Exp }
Exp : '(' Type ')' Term { CastExp $2 $4 } 
  | 'new' Id '(' ListTerm ')' { NewExp $2 $4 }


Type :: { Type }
Type : 'Object' { TypeObject } 
  | Id { TypeId $1 }


ListClassDecl :: { [ClassDecl] }
ListClassDecl : {- empty -} { [] } 
  | ListClassDecl ClassDecl { flip (:) $1 $2 }


ListFieldDecl :: { [FieldDecl] }
ListFieldDecl : {- empty -} { [] } 
  | ListFieldDecl FieldDecl { flip (:) $1 $2 }


ListMethodDecl :: { [MethodDecl] }
ListMethodDecl : {- empty -} { [] } 
  | ListMethodDecl MethodDecl { flip (:) $1 $2 }


ListField :: { [Field] }
ListField : {- empty -} { [] } 
  | Field { (:[]) $1 }
  | Field ',' ListField { (:) $1 $3 }


ListFormalArg :: { [FormalArg] }
ListFormalArg : {- empty -} { [] } 
  | FormalArg { (:[]) $1 }
  | FormalArg ',' ListFormalArg { (:) $1 $3 }


ListArg :: { [Arg] }
ListArg : {- empty -} { [] } 
  | Arg { (:[]) $1 }
  | Arg ',' ListArg { (:) $1 $3 }


ListAssignment :: { [Assignment] }
ListAssignment : {- empty -} { [] } 
  | ListAssignment Assignment { flip (:) $1 $2 }


ListTerm :: { [Term] }
ListTerm : {- empty -} { [] } 
  | Term { (:[]) $1 }
  | Term ',' ListTerm { (:) $1 $3 }



{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}

