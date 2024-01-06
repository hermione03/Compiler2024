open Ast
open Mips

module Env = Map.Make(String)

let _types_ = Env.empty

let builtins = 
   [
      
   ]
(* open Ast
open Mips

module Env = Map.Make(String)

let _types_ = Env.of_seq
(List.to_seq
   [ "%add", Func_t (Int_t, [ Int_t ; Int_t ])
])

let builtins = 
   [
      Label "%add"
      ; Lw (T0, Mem (SP, 0))
      ; Lw (T1, Mem (SP, 4))
      ; Add (V0, T0, T1)
      ; Jr RA
   ] *)
