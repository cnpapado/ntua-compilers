(* open Parser *)
open Lexing
(* open Ast *)
(* open Pretty_print *)
open Semantic

let get_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) 


let () =
let lexbuf = Lexing.from_channel stdin in
  try 
    let ast = Parser.program Lexer.lexer lexbuf in
    check_decl_list ast;
    exit 0
  with 
  | Parsing.Parse_error ->
    let err_msg = Printf.sprintf "%s: %s\n" (get_position lexbuf) "syntax error" in
    Printf.fprintf stderr "%s\n" err_msg ;
    exit 1  
  | Lexer.LexicalError msg -> 
    let err_msg = Printf.sprintf "%s: %s\n" (get_position lexbuf) msg in
    Printf.fprintf stderr "%s\n" err_msg ;
    exit 1

