%{
  open Ast
  open Ast.Syntax
%}

(*Values*)
%token <int> Lint
%token <bool> Lbool
%token <string> Lstring
%token <string> Lvar
%token <Ast.type_t> Ltype



(*Operators*)
%token Ladd Lsub Lmul Ldiv Lmod Lassign Lif Lelse Lwhile
%token Lgt Lgte Llt Llte Leq Lneq Land Lor 

(*Punctuations*)
%token Lsc Lc Lopar Lcpar Lobra Lcbra

(*Keywords*)
%token Lreturn Lend Lprint_int Lprint_str Lprint_bool Lprint_nl


%left Lor
%left Land


%left Leq Lneq Llt Lgt Llte Lgte

%left Ladd Lsub Lmod Lmul Ldiv




%start prog

%type <Ast.Syntax.prog> prog

%%

(*complete program*)
prog:
  | f = def ; p = prog { f @ p }
  | Lend { [] }


(*Function's definition*)
def:
| t=Ltype; fn = Lvar; Lopar; Lcpar; Lobra; b = block; Lcbra{
  [Func { type_t= t; name = fn ; args= [] ; block = b ;pos= $startpos(fn) }]
}
;

(*block of instructions*)
block:
  | Lend { [] }
  | i = instr; b = block { i::b }
;


(*instructions*)
instr:
  | Lreturn; e = expr; Lsc {
    Return {expr = e
            ; pos = $startpos($1) }}
  | id = Lvar;Lassign; e = expr; Lsc{
                  Assign {var = id
                          ; expr = e 
                          ; pos = $startpos(id)}}
  | t = Ltype; id = Lvar; Lsc {
    Decl { name=id ; type_t= t ; pos= $startpos(id) }
  }
  | e = expr; Lsc { 
      Expr { expr= e  ; pos= $startpos(e) }
  }
  | Lif; Lopar; expr; Lcpar; Lobra; block; Lcbra; Lelse; Lobra; block; Lcbra {
    Cond { test=$3 ; tblock=$6 ; fblock=$10 ; pos=$startpos($1) }
  }
;

(*expressions*)
expr:
  | v = value{
      Value { value= v ; pos= $startpos(v) }}
  | operator { $1 }
  | id = Lvar { 
      Var { name= id; pos= $startpos(id) }}
  
;

(*operators*)
operator:
  | expr ; Ladd ; expr {
      Call { func="%add" ; args=[$1;$3] ; pos=$startpos($2) }
  }
  | expr ; Lsub ; expr {
      Call { func="%sub" ; args=[$1;$3] ; pos=$startpos($2) }
  }
  | expr ; Lmul ; expr {
      Call { func="%mul" ; args=[$1;$3] ; pos=$startpos($2) }
  }
  | expr ; Lmod ; expr {
      Call { func="%mod" ; args=[$1;$3] ; pos=$startpos($2) }
  }
  | expr ; Ldiv ; expr {
      Call { func="%div" ; args=[$1;$3] ; pos=$startpos($2) }
  }
  | expr ; Lgt ; expr {
      Call { func="%gt"  ; args=[$1;$3] ; pos=$startpos($2) }
  }
  | expr ; Lgte ; expr {
      Call { func="%gte" ; args=[$1;$3] ; pos=$startpos($2) }
  }
  | expr ; Llt ; expr {
      Call { func="%lt"  ; args=[$1;$3] ; pos=$startpos($2) }
  }
  | expr ; Llte; expr {
      Call { func="%lte" ; args=[$1;$3] ; pos=$startpos($2) }
  }
  | expr ; Leq ; expr {
      Call { func="%eq"  ; args=[$1;$3] ; pos=$startpos($2) }
  }
  | expr ; Lneq ; expr {
      Call { func="%neq" ; args=[$1;$3] ; pos=$startpos($2) }
  }
  | expr ; Land ; expr {
      Call { func="%and" ; args=[$1;$3] ; pos=$startpos($2) }
  }
  | expr ; Lor ; expr {
      Call { func="%or"  ; args=[$1;$3] ; pos=$startpos($2) }
  }
  | Lprint_int; Lopar; expr; Lcpar {
      Call { func="puti" ; args=[$3] ; pos=$startpos($3) }
  }
  | Lprint_str; Lopar; expr; Lcpar {
      Call { func="puts" ; args=[$3] ; pos=$startpos($3) }
  }
  | Lprint_nl; Lopar; Lcpar {
      Call { func="putnl" ; args=[] ; pos=$startpos($1) }
  }
  | fn = Lvar; Lopar; Lcpar {
      Call { func= fn; args=[]; pos= $startpos(fn) }
  }
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