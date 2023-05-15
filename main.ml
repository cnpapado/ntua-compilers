open Parser
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let main =
  let lexbuf = Lexing.from_channel stdin in
  try
    let
      ast = Parser.program Lexer.lexer lexbuf
    in
    string_of_jumpname ast;
  
    exit 0
  with Parsing.Parse_error ->
    (* Printf.eprintf "syntax error\n"; *)
    Printf.fprintf stderr "syntax error %a\n" print_position lexbuf;
    exit 1 