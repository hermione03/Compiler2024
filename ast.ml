(* module type Parameters = sig
  type value
end *)

type type_t =
  | Void_t
  | Int_t
  | Str_t
  | Bool_t 
  | Func_t of type_t * type_t list

let rec string_of_type_t t =
  match t with
  | Void_t         -> "void"
  | Int_t         -> "int"
  | Bool_t        -> "bool"
  | Str_t         -> "str"
  | Func_t (r, a) ->
      (if (List.length a) > 1 then "(" else "")
      ^ (String.concat ", " (List.map string_of_type_t a))
      ^ (if (List.length a) > 1 then ")" else "")
      ^ " -> " ^ (string_of_type_t r)

module Syntax = struct
  type ident = string
  type value =
  | Void of { pos: Lexing.position}
  | Int of { value: int
           ; pos: Lexing.position}
  | Str of { value: string
           ; pos: Lexing.position}
  | Bool of { value: bool
            ; pos: Lexing.position}
  type expr =
    | Value of { value: value
               ; pos: Lexing.position }
    | Var   of { name: ident
               ; pos: Lexing.position }
    | Call  of { func: ident
               ; args: expr list
               ; pos: Lexing.position }
  type instr =
    | Expr   of { expr: expr
                ; pos: Lexing.position}
    (* | Decl   of { name: ident
                ; type_t: type_t
                ; pos: Lexing.position } *)
    | Decl of { name : string; pos : Lexing.position }
    | Assign of { var: ident
                ; expr: expr
                ; pos: Lexing.position }
    | Return of { expr: expr
                ; pos: Lexing.position }
  and block = instr list
end


(* module V1 = struct
  type value =
    | Void
    | Bool of bool
    | Int  of int
    | Str  of string
end *)

(*module V2 = struct
  type value =
    | Void
    | Bool of bool
    | Int  of int
    | Data of string
end *)


  
(* module IR (P : Parameters) = struct *)
module IR = struct
  type ident = string
  type value =
    | Void
    | Bool of bool
    | Int  of int
    | Str  of string
  type expr =
    | Value of value
    | Var   of ident
    | Call  of ident * expr list
  (* type lvalue =
    | LVar  of ident
    | LAddr of expr *)
  type instr =
    | Decl   of ident
    | Return of expr
    | Expr   of expr
    (* | Assign of Lvar * expr *)
    | Assign of ident * expr
    | Cond   of expr * block * block
  and block = instr list
  type def =
    | Func of ident * ident list * block
  type prog = def list
end
  
  (* module IR1 = IR(V1)
  module IR2 = IR(V2) *)