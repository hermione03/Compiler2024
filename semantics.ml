open Ast
open Ast.IR1
open Ast.V1
open Baselib

exception Error of string * Lexing.position



let expr_pos expr =
  match expr with
  | Syntax.Value v -> v.pos
  | Syntax.Var v   -> v.pos
  | Syntax.Call c  -> c.pos

let errt expected given pos =
  raise (Error (Printf.sprintf "expected %s but given %s"
                  (string_of_type_t expected)
                  (string_of_type_t given),
                pos))

(* analyse sémantique *)

let analyze_value value =
  match value with
  | Syntax.Void v -> Void, Void_t
  | Syntax.Int n  -> Int n.value, Int_t
  | Syntax.Bool b -> Bool b.value, Bool_t
  | Syntax.Str s  -> Str s.value, Str_t

let rec analyze_expr expr env =
  match expr with
  | Syntax.Value v ->
    let av,vt = (analyze_value v.value) in 
      Value (av),vt
  | Syntax.Var v ->
    if (Env.mem v.name env) then
      Var (v.name), Env.find v.name env 
    else
      raise (Error (Printf.sprintf "unbound variable \'%s\'" v.name, v.pos))
  (* ici il y aura des choses à ajouter *)
  | Syntax.Call c ->
    match Env.find_opt c.func env with
     | Some (Func_t (rt, at)) ->
        if List.length at != List.length c.args then
          raise (Error (Printf.sprintf "expected %d arguments but given %d"
                          (List.length at) (List.length c.args), c.pos)) ;
        let args = List.map2 (fun eat a -> let aa, at = analyze_expr a env
                                           in if at = eat then aa
                                              else errt eat at (expr_pos a))
                     at c.args in
        Call (c.func, args), rt
    |Some _ -> raise(Error (Printf.sprintf "'%s' is not a function" c.func, c.pos))
    |None -> raise(Error(Printf.sprintf "undefined function %s"c.func,c.pos))

let rec analyze_instr instr env stack=
  match instr with
  | Syntax.Expr e ->
    let ae,et = analyze_expr e.expr env in
    Expr ae, env
  | Syntax.Decl d ->
    IR1.Decl d.name, Env.add d.name d.type_t env
  (* | Syntax.Decl dv -> (Decl dv.name, env) *)
  | Syntax.Assign a ->
    if (Env.mem a.var env) then
      let vt = Env.find a.var env in 
      let ae,et = analyze_expr a.expr env in
      if (vt = et) then
        IR1.Assign (a.var, ae), env
      else
        errt vt et (expr_pos a.expr)
    else 
      raise(Error(Printf.sprintf "unbound variable %s"a.var,a.pos))

  | Syntax.Return r->
    let ae,et = analyze_expr r.expr env in
    let st = Pile.pop stack in
    if (et != st) then
      errt st et r.pos
    else
    IR1.Return ae, env
  | Syntax.Cond c -> 
    let t, et  = analyze_expr c.test env in
    let y, et2 = analyze_block c.tblock env stack in
    let n, et3 = analyze_block c.fblock et2 stack in
    IR1.Cond (t, y, n), et3

and analyze_block block env stack =
  match block with
  | [] -> [], env
  | instr :: rest ->
      let ai, new_env = analyze_instr instr env stack in
      let b, s_env = analyze_block rest new_env  stack in
      ai :: b , new_env

let analyze_func func env  stack =
  match func with
  | Syntax.Func f ->
    let args = [] in
    let st = Pile.push f.type_t stack in
    let block, new_env = analyze_block f.block env stack  in
    IR1.Func (f.type_t, f.name, args, block), 
              if (Env.mem f.name env) then
                raise (Error (Printf.sprintf "doesn't compile '%s' because this function exist with same name" 
                              f.name, f.pos))
              else
                (Env.add f.name (Func_t (Void_t, [])) env)

let rec analyze_prog prog env stack = 
  match prog with
  | [] -> [], env
  | func :: rest ->
     let ai, new_env = analyze_func func env stack in
     let bi, new_env2 = analyze_prog rest new_env  stack in
     ai :: bi, new_env2


let analyze parsed  =
  analyze_prog parsed Baselib._types_(Pile.create())
  (* analyze_block parsed Baselib._types_  *)

  