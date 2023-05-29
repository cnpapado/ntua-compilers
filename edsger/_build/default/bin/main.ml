open Parser
open Lexing
open Ast

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let main (s:string)=

  (*let lexbuf = Lexing.from_channel stdin*)
 let lexbuf = Lexing.from_string s in
  try
    let
      ast = Parser.program Lexer.lexer lexbuf
    in
        let 
          pp = Ast.string_of_expr_list ast
        in
          print_string pp;

    exit 0
  with Parsing.Parse_error ->
    (* Printf.eprintf "syntax error\n"; *)
    Printf.fprintf stderr "syntax error %a\n" print_position lexbuf;
    exit 1 

