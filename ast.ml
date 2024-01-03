module Syntax = struct
  type ident = string
  type value =
    | Bool of bool
    | Int  of int
  type expr =
    | Value of { value: value
               ; pos: Lexing.position }
    | Var   of { name: ident
               ; pos: Lexing.position }
    | Call  of { func: ident
               ; args: expr list
               ; pos: Lexing.position }
  type instr =
    | Decl   of { name: ident
                ; type_t: type_t
                ; pos: Lexing.position }
    | Assign of { var: ident
                ; expr: expr
                ; pos: Lexing.position }
  and block = instr list
end

module IR = struct
  type ident = string
  type value =
    | Bool of bool
    | Int  of int
  type expr =
    | Value of value
    | Var   of ident
    | Call  of ident * expr list
  type instr =
    | Decl   of ident
    | Assign of ident * expr
  and block = instr list
end
