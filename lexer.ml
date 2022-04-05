{
type token = 
    | T_bool | T_break | T_byref | T_char | T_continue | T_delete
    | T_double | T_else | T_for | T_false | T_if | T_int
    | T_new | T_NULL | T_return | T_true | T_void
    | T_intconst | T_realconst | T_id | T_charconst | T_stringconst
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let whitespace = [' ' '\t' '\r' '\n']
let ascii = ['0'-'9'|'A'-'F'|'a'-'f']
let ascii_escape = ['\x'](ascii)(ascii)
let esc_char = ['\t' '\r' '\n' '\0' '\'' '\"' '\\' | (ascii_escape)]
let const_char = [ _ | (esc_char)]

rule lexer = parse 
    "bool" { T_bool }
  | "break" { T_break } 
  | "byref" { T_byref }
  | "char" { T_char }
  | "continue" { T_continue }
  | "delete" { T_delete }
  | "double" { T_double }
  | "else" { T_else }
  | "for" { T_for }
  | "false" { T_false }
  | "if" { T_if }
  | "int" { T_int }
  | "new" { T_new }
  | "NULL" { T_NULL }
  | "return" { T_return }
  | "true" { T_true }
  | "void" { T_void }
  
  | '-'? digit+ { T_intconst }
  | '-'? digit+ '.' digit+ ( ['e' 'E'] ['+' '-']? digit+ )? {  T_realconst }
  | (letter)(letter|digit|'_')* {T_id}
  
  | '\'' (const_char) '\'' {T_charconst}
  |'\"' [(const_char)+ | (esc_char)]+ '\"' {T_stringconst}
  (* | '\"' [^ '"' '\\']+ '\"' {T_stringconst} *)

  

(* char, string *)
(* symbolikoi telestes, diaxwristes ws token me onoma thn timh toy *)

(* to ignore 
whitespaces
comments
*)
(* pointers !! *)


(*
{ 
type token =  
}
B 
{
    C 
}
*)

{
  let string_of_token token =
    match token with
      | T_bool     -> "T_bool"
      | T_break    -> "T_break"
      | T_byref    -> "T_byref"
      | T_char     -> "T_char"
      | T_continue -> "T_continue"
      | T_delete   -> "T_delete"
      | T_double   -> "T_double"
      | T_else     -> "T_else"
      | T_for    -> "T_for"
      | T_false     -> "T_false"
      | T_if   -> "T_if"
      | T_new     -> "T_new"
      | T_NULL -> "T_NULL"
      | T_return -> "T_return"
      | T_true   -> "T_true"
      | T_void  -> "T_void"
      | T_intconst  -> "T_intconst"
      | T_realconst  -> "T_realconst"
      | T_id  -> "T_id"
      | T_charconst  -> "T_charconst"
      | T_stringconst  -> "T_stringconst"


  let main =
    let lexbuf = Lexing.from_channel stdin in
    let rec loop () =
      let token = lexer lexbuf in
      Printf.printf "token=%s, lexeme=\"%s\"\n"
        (string_of_token token) (Lexing.lexeme lexbuf);
      if token <> T_eof then loop () in
    loop ()
}

