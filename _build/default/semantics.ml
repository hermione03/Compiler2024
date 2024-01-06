open Ast

open Baselib

exception Error of string * Lexing.position

module Env = Map.Make(String)
(* fonctions d'aide à la gestion des erreurs *)

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
    |Some (Func_t (rt,at)) ->
      let args = List.map (fun a -> fst (analyze_expr a env)) c.args in
      Call (c.func, args),rt
    |Some _ -> raise(Error (Printf.sprintf "'%s' is not a function" c.func, c.pos))
    |None -> raise(Error(Printf.sprintf "undefined function %s"c.func,c.pos))

let analyze_instr instr env =
  match instr with
  | Syntax.Expr e ->
    let ae,et = analyze_expr e.expr env in
    Expr ae, env
  | Syntax.Decl d ->
    Decl d.name, Env.add d.name d.type_t env
  | Syntax.Assign a ->
    if (Env.mem a.var env) then
      let vt = Env.find a.var env in 
      let ae,et = analyze_expr a.expr env in
      if (vt = et) then
        Assign (a.var, ae), env
      else
        errt vt et (expr_pos a.expr)
    else 
      raise(Error(Printf.sprintf "unbound variable %s"a.var,a.pos))
  (*modif au niveau du return --> la pile*)
  | Syntax.Return r->
    let ae,et = analyze_expr r.expr env in
    Return ae, env

(*ajouter la pile*)
let rec analyze_block block env =
  match block with
  | [] -> [], env
  | instr :: rest ->
    let ai,nenv = analyze_instr instr env in
    ai :: (analyze_block rest nenv)


(*analyse func*)
(*analyse prog*)


let analyze parsed =
  analyze_block parsed _types_ 

