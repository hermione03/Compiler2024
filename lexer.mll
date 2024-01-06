{
  open Lexing
  open Parser

  exception Error of char
}

let num = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha ( alpha | num | '_')
let bool  = "true" | "false"

rule token = parse
| eof             { Lend }
| [ ' ' '\t' ]    { token lexbuf } (*j'gnore et passe à la suite *)
| '\n'            { Lexing.new_line lexbuf; token lexbuf } (*saute une ligne -> incremante le numero de ligne pas fait par def*)
| "//"             { comment lexbuf } (*si eof lend si nl bah incremante compteuir de lignes dans lexbuff puis appele le prochain token -> va jusqu'a la fin de la ligne / fichier et se rappel recursivement prochain token *)
| "/*"            { comment_block lexbuf }
| ';'             { Lsc }


(*types*)

| "int"           { Ltype (Int_t) }
| "str"           { Ltype (Str_t) }
| "bool"          { Ltype (Bool_t) }
| "void"          { Ltype (Nil_t) }

| "return"      { Lreturn }
| "="             { Lassign}

(*operateurs de base*)
| "+"             { Ladd}
| "-"             { Lsub}
| "*"             { Lmul}
| "/"             { Ldiv}
| '%'           { Lmod }

(* braces *)
| "{"             { Locbra }
| "}"             { Lcabra }
(* parenthesises *)
| "("             { Lopar}
| ")"             { Lcpar}

(*logic*)
| "&&"            { Land }
| "||"            { Lor }

(*comparaisons*)
| "=="          { Lseq }
| ">="          { Lsge }
| ">"           { Lsgt }
| "<="          { Lsle }
| "<"           { Lslt }
| "!="          { Lsne }


| alpha+ as char  { Lvar char }
| num+ as n       { Lint(int_of_string n) }
| ident+ as id    {Lident id}
| bool as b     { Lbool (bool_of_string b) }
| _ as c          { raise (Error c) }

and comment = parse
| eof  { Lend }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| _    { comment lexbuf }

and comment_block = parse
| eof  { Lend }
| "*/" { Lexing.new_line lexbuf; token lexbuf }
| _    { comment_block lexbuf }