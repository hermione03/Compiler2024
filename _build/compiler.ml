open Mips
open Ast
open Ast.IR

(* open Ast.IR2
open Ast.V2 *)

module Env = Map.Make(String)
exception Error of string

type cinfo = { asm: Mips.instr list
             ; env: Mips.loc Env.t
             ; fpo: int
             ; counter: int
             ; return: string }

let compile_value v =
  match v with
  | Void    -> [ Li (V0, 0) ]
  | Bool b -> [ Li (V0, if b then 1 else 0) ]
  | Int n  -> [ Li (V0, n) ]
  | Str s -> [ La (V0, Lbl s) ]
  (* | Data d -> [ La (V0, Lbl d) ] *)

let rec compile_expr e env =
  match e with
  | Value v -> compile_value v
  | Var v   -> [ Lw (V0, Env.find v env) ]
  | Call (f, args) ->
    let ca = List.rev_map (fun a ->
        compile_expr a env
        @ [ Addi (SP, SP, -4)
          ; Sw (V0, Mem (SP, 0)) ])
        args in
    List.flatten ca
    @ [ Jal f
      ; Addi (SP, SP, 4 * (List.length args)) ]

let rec compile_instr i info env =
  match i with
  | Decl v ->
    { info with
      env = Env.add v (Mem (FP, -info.fpo)) info.env
    ; fpo = info.fpo + 4 }
  | Return e ->
    { info with
      asm = info.asm
             @ compile_expr e info.env
             @ [ B info.return ] }
  | Expr e ->
    { info with
      asm = info.asm
             @ compile_expr e info.env }
  (* | Assign (lv, e) ->
    { info with
      asm = info.asm
             @ compile_expr e info.env
             @ (match lv with
                 | LVar  v -> [ Sw (V0, Env.find v info.env) ]
                 | LAddr a -> []
                              @ [ Addi (SP, SP, -4)
                                ; Sw (V0, Mem (SP, 0)) ]
                              @ compile_expr a info.env
                              @ [ Lw (T0, Mem (SP, 0))
                                ; Addi (SP, SP, 4)
                                ; Sw (T0, Mem (V0, 0)) ]) } *)
  | Assign (lv, e) ->
    { info with
      asm =
        (info.asm
        @ compile_expr e info.env
        @ [ Sw (V0, Env.find lv info.env) ])
    }
  | Cond (c, t, e) ->
    let uniq = string_of_int info.counter in
    let ct = compile_block t { info with asm = []
                                       ; counter = info.counter + 1 } env in
    let ce = compile_block e { info with asm = []
                                       ; counter = ct.counter + 1 } env in
    { info with
      asm = info.asm
             @ compile_expr c info.env
             @ [ Beqz (V0, "else" ^ uniq) ]
             @ ct.asm
             @ [ B ("endif" ^ uniq)
               ; Label ("else" ^ uniq) ]
             @ ce.asm
             @ [ Label ("endif" ^ uniq) ]
    ; counter = ce.counter }

and compile_block b info env =
  match b with
  | [] -> info
  | i :: r ->
    compile_block r (compile_instr i info env) env

let compile_def (Func (name, args, b)) counter env =
  let cb = compile_block b
      { asm = []
      ; env =  List.fold_left
            (fun e (i, a) -> Env.add a (Mem (FP, 4 * i)) e)
            Env.empty (List.mapi (fun i a -> i + 1, a) args)
      ; fpo = 8
      ; counter = counter + 1
      ; return = "ret" ^ (string_of_int counter) }
      env
  in cb.counter,
     []
     @ [ Label name
       ; Addi (SP, SP, -cb.fpo)
       ; Sw (RA, Mem (SP, cb.fpo - 4))
       ; Sw (FP, Mem (SP, cb.fpo - 8))
       ; Addi (FP, SP, cb.fpo - 4) ]
     @ cb.asm
     @ [ Label cb.return
       ; Addi (SP, SP, cb.fpo)
       ; Lw (RA, Mem (FP, 0))
       ; Lw (FP, Mem (FP, -4))
       ; Jr (RA) ]

let rec compile_prog p counter =
match p with
| [] -> []
| d :: r ->
  let new_counter, cd = compile_def d counter in
  cd @ (compile_prog r new_counter)

(* let compile (code, data) =
  { text = Baselib.builtins @ compile_prog code 0
  ; data = List.map (fun (l, s) -> (l, Asciiz s)) data } *)

let compile ir =
  { text = Baselib.builtins @ compile_expr ir
  ; data = [] }