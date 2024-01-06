
(* The type of tokens. *)

type token = 
  | Lvar
  | Ltype of (Ast.type_t)
  | Lstring of (string)
  | Lsc
  | Lreturn
  | Lint of (int)
  | Lident of (string)
  | Leq
  | Lend
  | Lbool of (bool)
  | Lassign

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.Syntax.block)
