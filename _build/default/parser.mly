%{
  open Ast
  open Ast.Syntax
%}

%token <int> Lint
%token <bool> Lbool
%token <string> Lstring
%token <string> LData
%token Lend

%start prog

%type <Ast.Syntax.expr> prog

%%

prog:
| e = expr; Lend { e }
;

expr:
| v = value{
    Value { value= v ; pos= $startpos(v) }
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