%{
  open Ast
  open Ast.Syntax
%}

%token <int> Lint
%token <bool> Lbool
%token <string> Lstring
%token <string> Lident
%token <Ast.type_t> Ltype
%token Lvar 


(*Operators*)
%token Ladd Lsub Lmul Ldiv Lmod Lassign Lif Lelse Lwhile
%token Lgt Lgte Llt Llte Leq Lneq Land Lor 

(*Punctuations*)
%token Lsc Lopar Lcpar Lobra Lcbra

(*Keywords*)
%token Lreturn Lend Lprint_int Lprint_str Lprint_bool Lprint_nl

(*Values*)
// %token <int> Lint
// %token <bool> Lbool
// %token <string> Lvar
// %token <string> Lstring
// %token <Ast.type_t> Ltype


%left Lor
%left Land


%left Leq Lneq Llt Lgt Llte Lgte

%left Ladd Lsub Lmod Lmul Ldiv


%start prog

%type <Ast.Syntax.expr> prog

%%

block:
| i = instr ; Lsc ; b = block { i @ b }
| i = instr ; Lsc { i }
;

prog:
	| e = expr; Lend { e }
;
// prog:
// 	| i = instr ; Lsc ; b = prog { i @ b }
// 	| i = instr ; Lsc ; Lend { i }
// ;

// instr:
//   | Lvar; id = Lident 
//   {
//     [Decl { name = id ; pos = $startpos(id)}]
//   }
//   | Lvar; Lassign; expr; Lsc {
//     Assign { var=$1 ; expr=$3 ; pos=$startpos($1) }
// }
//   /* On renvoi des listes  */
//   | Lvar; id = Lident ; Leq; e = expr 
//   {
//     [ Decl { name = id ; pos = $startpos(id)}
//       ;Assign { var = id
//       ; expr = e ; pos = $startpos($3)}
//     ]
//   }
//   | id = Lident; Leq; e = expr{
//     [Assign { var = id
//       ; expr = e 
//       ; pos = $startpos($2)
//       }
//     ]
//   }

(*Instruction*)
instr:
| Lvar; id = Lident 
  {
   [ Decl { name = id ; pos = $startpos(id)}]
  }
  | Lvar; id = Lident; Leq; e = expr
  {
    [ Decl { name = id ; pos = $startpos(id)}
      ; Assign { var = id ; expr = e ; pos = $startpos($3) }
    ]
  }
  | id = Lident; Leq; e = expr
  {
	[ Assign { var = id
     		 ; expr = e 
    		 ; pos = $startpos($2) 
    		 }
    ]
  }
  | Lreturn; e = expr { [ Return { expr = e; pos = $startpos($1) } ] }

;


expr:
| v = value{
    Value { value= v ; pos= $startpos(v) }}
;

(*Values*)
value:
| n = Lint {
  Int { value = n ; pos = $startpos(n) }
}
| b = Lbool {
  Bool { value = b ; pos = $startpos(b) }}
| s = Lstring {
  Str { value = s ; pos = $startpos(s) }}
;