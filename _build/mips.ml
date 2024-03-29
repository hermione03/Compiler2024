type reg =
  | Zero
  | SP
  | RA
  | FP
  | V0
  | A0
  | A1
  | T0
  | T1
  | T2
  | T3
  | T4

type label = string

type loc =
  | Lbl of label
  | Reg of reg
  | Mem of reg * int

type instr =
  | Label of label
  | Li    of reg * int
  | La    of reg * loc
  | Sw    of reg * loc
  | Lw    of reg * loc
  | Sb    of reg * loc
  | Lb    of reg * loc
  | Sll  of reg * reg * int 
  | Srl  of reg * reg * int 
  | Move  of reg * reg

  | Addi  of reg * reg * int
  | Add   of reg * reg * reg
  | Sub   of reg * reg * reg
  | Mul   of reg * reg * reg
  | Div   of reg * reg * reg

  | And  of reg * reg * reg
  | Andi  of reg * reg * int
  | Xor   of reg * reg * reg
  | Or   of reg * reg * reg

  | Syscall
  | B     of label
  | Beq   of reg * reg * label
  | Bne   of reg * reg * label
  | Beqz  of reg * label
  | Bgeu  of reg * reg * label 
  | Bgt  of reg * reg * label 
  | Bltu  of reg * reg * label 
  | Blez  of reg * label 
  | Ble   of reg * int 
  | Jal   of label
  | Jr    of reg



type directive =
  | Asciiz of string

type decl = label * directive

type asm = { text: instr list ; data: decl list }

module Syscall = struct
  let print_int = 1
  let print_str = 4
  let read_int = 5
  let read_str = 8
  let sbrk = 9
end

let ps = Printf.sprintf (* alias raccourci *)

let fmt_reg = function
  | Zero -> "$zero"
  | SP   -> "$sp"
  | RA   -> "$ra"
  | V0   -> "$v0"
  | A0   -> "$a0"
  | A1   -> "$a1"
  | T0   -> "$t0"
  | T1   -> "$t1"
  | T2   -> "$t2"
  | T3   -> "$t3"
  | T4   -> "$t4"

let fmt_loc = function
| Lbl (l)    -> l
| Reg (r)    -> fmt_reg r
| Mem (r, o) -> ps "%d(%s)" o (fmt_reg r)

  let fmt_instr = function
  | Label (l)        -> ps "%s:" l
  | Li (r, i)        -> ps "  li %s, %d" (fmt_reg r) i
  | La (r, a)        -> ps "  la %s, %s" (fmt_reg r) (fmt_loc a)
  | Sw (r, a)        -> ps "  sw %s, %s" (fmt_reg r) (fmt_loc a)
  | Lw (r, a)        -> ps "  lw %s, %s" (fmt_reg r) (fmt_loc a)
  | Sb (r, a)        -> ps "  sb %s, %s" (fmt_reg r) (fmt_loc a)
  | Lb (r, a)        -> ps "  lb %s, %s" (fmt_reg r) (fmt_loc a)
  | Sll (rd, rs, i) -> ps "  sll %s, %s, %d" (fmt_reg rd) (fmt_reg rs) i
  | Srl (rd, rs, i) -> ps "  srl %s, %s, %d" (fmt_reg rd) (fmt_reg rs) i
  | Move (rd, rs)    -> ps "  move %s, %s" (fmt_reg rd) (fmt_reg rs)
  | Addi (rd, rs, i) -> ps "  addi %s, %s, %d" (fmt_reg rd) (fmt_reg rs) i
  | Add (rd, rs, rt) -> ps "  add %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Sub (rd, rs, rt) -> ps "  sub %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Mul (rd, rs, rt) -> ps "  mul %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Div (rd, rs, rt) -> ps "  div %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | And (rd, rs, rt) -> ps "  and %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Andi (rd, rs, i) -> ps "  andi %s, %s, %d" (fmt_reg rd) (fmt_reg rs) i
  | Or (rd, rs, rt) -> ps "  or %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Xor (rd, rs, rt) -> ps "  xor %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Syscall          -> ps "  syscall"
  | B (l)            -> ps "  b %s" l
  | Beq (rs, rt, l)  -> ps "  beq %s, %s, %s" (fmt_reg rs) (fmt_reg rt) l
  | Bne (rs, rt, l)  -> ps "  bne %s, %s, %s" (fmt_reg rs) (fmt_reg rt) l
  | Bgeu(rs, rt , l) -> ps "  bgeu %s, %s, %s" (fmt_reg rs) (fmt_reg rt) l
  | Bgt(rs, rt , l) -> ps "  bgt %s, %s, %s" (fmt_reg rs) (fmt_reg rt) l
  | Bltu(rs, rt , l) -> ps "  blu %s, %s, %s" (fmt_reg rs) (fmt_reg rt) l
  | Blez(rs, l)      -> ps "  blez %s, %s" (fmt_reg rs) l
  | Ble(rs, i)      -> ps "  ble %s, %d" (fmt_reg rs) i
  | Jal (l)          -> ps "  jal %s" l
  | Jr (r)           -> ps "  jr %s" (fmt_reg r)



let fmt_dir = function
  | Asciiz (s) -> ps ".asciiz \"%s\"" s

let emit oc asm =
  Printf.fprintf oc ".text\n.globl main\nmain:\n" ;
  List.iter (fun i -> Printf.fprintf oc "%s\n" (fmt_instr i)) asm.text ;
  Printf.fprintf oc "  move $a0, $v0\n  li $v0, 1\n  syscall\n  jr $ra\n" ;
  Printf.fprintf oc "\n.data\n" ;
  List.iter (fun (l, d) -> Printf.fprintf oc "%s: %s\n" l (fmt_dir d)) asm.data
