
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | Lvar
    | Ltype of (
# 10 "parser.mly"
       (Ast.type_t)
# 16 "parser.ml"
  )
    | Lstring of (
# 8 "parser.mly"
       (string)
# 21 "parser.ml"
  )
    | Lsc
    | Lreturn
    | Lint of (
# 6 "parser.mly"
       (int)
# 28 "parser.ml"
  )
    | Lident of (
# 9 "parser.mly"
       (string)
# 33 "parser.ml"
  )
    | Leq
    | Lend
    | Lbool of (
# 7 "parser.mly"
       (bool)
# 40 "parser.ml"
  )
    | Lassign
  
end

include MenhirBasics

# 1 "parser.mly"
  
  open Ast
  open Ast.Syntax

# 53 "parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_prog) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: prog. *)

  | MenhirState03 : (('s, _menhir_box_prog) _menhir_cell1_Lvar _menhir_cell0_Lident _menhir_cell0_Leq, _menhir_box_prog) _menhir_state
    (** State 03.
        Stack shape : Lvar Lident Leq.
        Start symbol: prog. *)

  | MenhirState09 : (('s, _menhir_box_prog) _menhir_cell1_Lreturn, _menhir_box_prog) _menhir_state
    (** State 09.
        Stack shape : Lreturn.
        Start symbol: prog. *)

  | MenhirState12 : (('s, _menhir_box_prog) _menhir_cell1_Lident _menhir_cell0_Leq, _menhir_box_prog) _menhir_state
    (** State 12.
        Stack shape : Lident Leq.
        Start symbol: prog. *)

  | MenhirState16 : (('s, _menhir_box_prog) _menhir_cell1_instr, _menhir_box_prog) _menhir_state
    (** State 16.
        Stack shape : instr.
        Start symbol: prog. *)


and ('s, 'r) _menhir_cell1_instr = 
  | MenhirCell1_instr of 's * ('s, 'r) _menhir_state * (Ast.Syntax.block)

and 's _menhir_cell0_Leq = 
  | MenhirCell0_Leq of 's * Lexing.position

and ('s, 'r) _menhir_cell1_Lident = 
  | MenhirCell1_Lident of 's * ('s, 'r) _menhir_state * (
# 9 "parser.mly"
       (string)
# 92 "parser.ml"
) * Lexing.position

and 's _menhir_cell0_Lident = 
  | MenhirCell0_Lident of 's * (
# 9 "parser.mly"
       (string)
# 99 "parser.ml"
) * Lexing.position

and ('s, 'r) _menhir_cell1_Lreturn = 
  | MenhirCell1_Lreturn of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_Lvar = 
  | MenhirCell1_Lvar of 's * ('s, 'r) _menhir_state

and _menhir_box_prog = 
  | MenhirBox_prog of (Ast.Syntax.block) [@@unboxed]

let _menhir_action_01 =
  fun _startpos_v_ v ->
    (
# 80 "parser.mly"
           (
    Value { value= v ; pos= _startpos_v_ }
)
# 118 "parser.ml"
     : (Ast.Syntax.expr))

let _menhir_action_02 =
  fun _startpos_id_ id ->
    (
# 57 "parser.mly"
  (
   [ Decl { name = id ; pos = _startpos_id_}]
  )
# 128 "parser.ml"
     : (Ast.Syntax.block))

let _menhir_action_03 =
  fun _startpos__3_ _startpos_id_ e id ->
    (
# 61 "parser.mly"
  (
    [ Decl { name = id ; pos = _startpos_id_}
      ; Assign { var = id ; expr = e ; pos = _startpos__3_ }
    ]
  )
# 140 "parser.ml"
     : (Ast.Syntax.block))

let _menhir_action_04 =
  fun _startpos__2_ e id ->
    (
# 67 "parser.mly"
  (
	[ Assign { var = id
     		 ; expr = e 
    		 ; pos = _startpos__2_ 
    		 }
    ]
  )
# 154 "parser.ml"
     : (Ast.Syntax.block))

let _menhir_action_05 =
  fun _startpos__1_ e ->
    (
# 74 "parser.mly"
                      ( [ Return { expr = e; pos = _startpos__1_ } ] )
# 162 "parser.ml"
     : (Ast.Syntax.block))

let _menhir_action_06 =
  fun b i ->
    (
# 26 "parser.mly"
                              ( i @ b )
# 170 "parser.ml"
     : (Ast.Syntax.block))

let _menhir_action_07 =
  fun i ->
    (
# 27 "parser.mly"
                          ( i )
# 178 "parser.ml"
     : (Ast.Syntax.block))

let _menhir_action_08 =
  fun _startpos_n_ n ->
    (
# 87 "parser.mly"
           (
  Int { value = n ; pos = _startpos_n_ }
)
# 188 "parser.ml"
     : (Ast.Syntax.value))

let _menhir_action_09 =
  fun _startpos_b_ b ->
    (
# 90 "parser.mly"
            (
  Bool { value = b ; pos = _startpos_b_ })
# 197 "parser.ml"
     : (Ast.Syntax.value))

let _menhir_action_10 =
  fun _startpos_s_ s ->
    (
# 92 "parser.mly"
              (
  Str { value = s ; pos = _startpos_s_ })
# 206 "parser.ml"
     : (Ast.Syntax.value))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | Lassign ->
        "Lassign"
    | Lbool _ ->
        "Lbool"
    | Lend ->
        "Lend"
    | Leq ->
        "Leq"
    | Lident _ ->
        "Lident"
    | Lint _ ->
        "Lint"
    | Lreturn ->
        "Lreturn"
    | Lsc ->
        "Lsc"
    | Lstring _ ->
        "Lstring"
    | Ltype _ ->
        "Ltype"
    | Lvar ->
        "Lvar"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_14 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_prog =
    fun _menhir_stack _v ->
      MenhirBox_prog _v
  
  let rec _menhir_goto_prog : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState16 ->
          _menhir_run_18 _menhir_stack _v
      | MenhirState00 ->
          _menhir_run_14 _menhir_stack _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_18 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_instr -> _ -> _menhir_box_prog =
    fun _menhir_stack _v ->
      let MenhirCell1_instr (_menhir_stack, _menhir_s, i) = _menhir_stack in
      let b = _v in
      let _v = _menhir_action_06 b i in
      _menhir_goto_prog _menhir_stack _v _menhir_s
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Lident _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Leq ->
              let _menhir_stack = MenhirCell1_Lvar (_menhir_stack, _menhir_s) in
              let _menhir_stack = MenhirCell0_Lident (_menhir_stack, _v, _startpos) in
              let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
              let _menhir_stack = MenhirCell0_Leq (_menhir_stack, _startpos) in
              let _menhir_s = MenhirState03 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | Lstring _v ->
                  _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | Lint _v ->
                  _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | Lbool _v ->
                  _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | Lsc ->
              let (_startpos_id_, id) = (_startpos, _v) in
              let _v = _menhir_action_02 _startpos_id_ id in
              _menhir_goto_instr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_04 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_startpos_s_, s) = (_startpos, _v) in
      let _v = _menhir_action_10 _startpos_s_ s in
      _menhir_goto_value _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_s_ _v _menhir_s _tok
  
  and _menhir_goto_value : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      let (_startpos_v_, v) = (_startpos, _v) in
      let _v = _menhir_action_01 _startpos_v_ v in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState12 ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState09 ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState03 ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_13 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_Lident _menhir_cell0_Leq -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_Leq (_menhir_stack, _startpos__2_) = _menhir_stack in
      let MenhirCell1_Lident (_menhir_stack, _menhir_s, id, _) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_04 _startpos__2_ e id in
      _menhir_goto_instr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_instr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Lsc ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Lvar ->
              let _menhir_stack = MenhirCell1_instr (_menhir_stack, _menhir_s, _v) in
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
          | Lreturn ->
              let _menhir_stack = MenhirCell1_instr (_menhir_stack, _menhir_s, _v) in
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
          | Lident _v_0 ->
              let _menhir_stack = MenhirCell1_instr (_menhir_stack, _menhir_s, _v) in
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState16
          | Lend ->
              let i = _v in
              let _v = _menhir_action_07 i in
              _menhir_goto_prog _menhir_stack _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_09 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_Lreturn (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState09 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Lstring _v ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Lint _v ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Lbool _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_05 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_startpos_n_, n) = (_startpos, _v) in
      let _v = _menhir_action_08 _startpos_n_ n in
      _menhir_goto_value _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_n_ _v _menhir_s _tok
  
  and _menhir_run_06 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_startpos_b_, b) = (_startpos, _v) in
      let _v = _menhir_action_09 _startpos_b_ b in
      _menhir_goto_value _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_b_ _v _menhir_s _tok
  
  and _menhir_run_11 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_Lident (_menhir_stack, _menhir_s, _v, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Leq ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _menhir_stack = MenhirCell0_Leq (_menhir_stack, _startpos) in
          let _menhir_s = MenhirState12 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Lstring _v ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | Lint _v ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | Lbool _v ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_10 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_Lreturn -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_Lreturn (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_05 _startpos__1_ e in
      _menhir_goto_instr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_08 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_Lvar _menhir_cell0_Lident _menhir_cell0_Leq -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_Leq (_menhir_stack, _startpos__3_) = _menhir_stack in
      let MenhirCell0_Lident (_menhir_stack, id, _startpos_id_) = _menhir_stack in
      let MenhirCell1_Lvar (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_03 _startpos__3_ _startpos_id_ e id in
      _menhir_goto_instr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState00 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Lvar ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Lreturn ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Lident _v ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
end

let prog =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_prog v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
