
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | Lstring of (
# 8 "parser.mly"
       (string)
# 15 "parser.ml"
  )
    | Lint of (
# 6 "parser.mly"
       (int)
# 20 "parser.ml"
  )
    | Lend
    | Lbool of (
# 7 "parser.mly"
       (bool)
# 26 "parser.ml"
  )
    | LData of (
# 9 "parser.mly"
       (string)
# 31 "parser.ml"
  )
  
end

include MenhirBasics

# 1 "parser.mly"
  
  open Ast
  open Ast.Syntax

# 43 "parser.ml"

type ('s, 'r) _menhir_state

and _menhir_box_prog = 
  | MenhirBox_prog of (Ast.Syntax.expr) [@@unboxed]

let _menhir_action_1 =
  fun _startpos_v_ v ->
    (
# 23 "parser.mly"
           (
    Value { value= v ; pos= _startpos_v_ }
)
# 57 "parser.ml"
     : (Ast.Syntax.expr))

let _menhir_action_2 =
  fun e ->
    (
# 19 "parser.mly"
                 ( e )
# 65 "parser.ml"
     : (Ast.Syntax.expr))

let _menhir_action_3 =
  fun _startpos_n_ n ->
    (
# 30 "parser.mly"
           (
  Int { value = n ; pos = _startpos_n_ }
)
# 75 "parser.ml"
     : (Ast.Syntax.value))

let _menhir_action_4 =
  fun _startpos_b_ b ->
    (
# 33 "parser.mly"
            (
  Bool { value = b ; pos = _startpos_b_ })
# 84 "parser.ml"
     : (Ast.Syntax.value))

let _menhir_action_5 =
  fun _startpos_s_ s ->
    (
# 35 "parser.mly"
              (
  Str { value = s ; pos = _startpos_s_ })
# 93 "parser.ml"
     : (Ast.Syntax.value))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | LData _ ->
        "LData"
    | Lbool _ ->
        "Lbool"
    | Lend ->
        "Lend"
    | Lint _ ->
        "Lint"
    | Lstring _ ->
        "Lstring"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_goto_value : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _startpos _v _tok ->
      let (_startpos_v_, v) = (_startpos, _v) in
      let _v = _menhir_action_1 _startpos_v_ v in
      match (_tok : MenhirBasics.token) with
      | Lend ->
          let e = _v in
          let _v = _menhir_action_2 e in
          MenhirBox_prog _v
      | _ ->
          _eRR ()
  
  let _menhir_run_0 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Lstring _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_startpos_s_, s) = (_startpos, _v) in
          let _v = _menhir_action_5 _startpos_s_ s in
          _menhir_goto_value _menhir_stack _startpos_s_ _v _tok
      | Lint _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_startpos_n_, n) = (_startpos, _v) in
          let _v = _menhir_action_3 _startpos_n_ n in
          _menhir_goto_value _menhir_stack _startpos_n_ _v _tok
      | Lbool _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_startpos_b_, b) = (_startpos, _v) in
          let _v = _menhir_action_4 _startpos_b_ b in
          _menhir_goto_value _menhir_stack _startpos_b_ _v _tok
      | _ ->
          _eRR ()
  
end

let prog =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_prog v = _menhir_run_0 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
