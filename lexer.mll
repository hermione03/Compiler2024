{
  open Lexing
  open Parser

  exception Error of char
  exception StrEndError of string
}

let num = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | num | '-' | '_')*
let bool  = "true" | "false"

rule token = parse
| eof             { Lend }
| [ ' ' '\t' ]    { token lexbuf } (*j'gnore et passe à la suite *)
| '\n'            { Lexing.new_line lexbuf; token lexbuf } (*saute une ligne -> incremante le numero de ligne pas fait par def*)
| "//"             { comment lexbuf } (*si eof lend si nl bah incremante compteuir de lignes dans lexbuff puis appele le prochain token -> va jusqu'a la fin de la ligne / fichier et se rappel recursivement prochain token *)
| "/*"            { comment_block lexbuf }
| ';'             { Lsc }
| ','             { Lc }
| '"'             { Lstring (String.concat "" (string_form lexbuf))}

(*types*)

| "int"           { Ltype (Int_t) }
| "str"           { Ltype (Str_t) }
| "bool"          { Ltype (Bool_t) }
| "void"          { Ltype (Void_t) }
| "true"          { Lbool true }
| "false"         { Lbool false }

| "return"        { Lreturn }
| "="             { Lassign}

(*operators*)
| "+"             { Ladd}
| "-"             { Lsub}
| "*"             { Lmul}
| "/"             { Ldiv}
| '%'             { Lmod }

(* braces *)
| "{"             { Lobra }
| "}"             { Lcbra }
(* parenthesises *)
| "("             { Lopar}
| ")"             { Lcpar}

(*logic*)
| "&&"            { Land }
| "||"            { Lor }

(*comparaisons*)
| "=="          { Leq }
| ">="          { Lgte }
| ">"           { Lgt }
| "<="          { Llte }
| "<"           { Llt }
| "!="          { Lneq }


(* | alpha+ as char  { Lident char } *)
| num+ as n       { Lint(int_of_string n) }
| ident as id    {Lvar id}
| bool as b     { Lbool (bool_of_string b) }
| _ as c          { raise (Error c) }

and string_form = parse
| eof             { raise (StrEndError "Missing '") }
| '"'             { [] }
| "\\n"           { "\n" :: (string_form lexbuf) }
| "\\t"           { "\t" :: (string_form lexbuf) }
| _ as c          { (String.make 1 c) :: (string_form lexbuf) }


and comment = parse
| eof  { Lend }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| _    { comment lexbuf }

and comment_block = parse
| eof  { Lend }
| "*/" { Lexing.new_line lexbuf; token lexbuf }
| _    { comment_block lexbuf }