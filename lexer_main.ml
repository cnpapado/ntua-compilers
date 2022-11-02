open Parser
open Lexing

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
    | T_intconst(int)  -> "T_intconst"
    | T_doubleconst(double)  -> "T_doubleconst"
    | T_id  -> "T_id"
    | T_charconst(char)  -> "T_charconst"
    | T_stringliteral(string)  -> "T_stringliteral"
    | T_int -> "T_int"
    | T_eof -> "EOF"
    | _ -> "special symbol"  


let main =
  let lexbuf = Lexing.from_channel stdin in
  let rec loop () =
    let token = Lexer.lexer lexbuf in
    Printf.printf "token=%s, lexeme=\"%s\"\n"
      (string_of_token token) (Lexing.lexeme lexbuf);
    if token <> T_eof then loop () in
  loop ()