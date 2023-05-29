(* open Parser *)
open Lexing
(* open Ast *)
(* open Pretty_print *)

let get_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) 


(* 

(* Iterates the ast and applies f into each node
 * Might need a better, more functional way to do it. *)
let rec iter_ast root f = 
match root with  
| h::tl -> f h; iter_ast tl f; ()
| [] -> print_string "done" ; ()
  (* | [Ast.FuncDef(_)] -> f root; () *)
  (* | [] -> f  *)
  (* |  -> f "a" *)

let pp_node node = 
match node with
  (* | h::tl -> pp_ast h ; pp_ast tl ; () *)
  | Ast.FuncDef(x) -> print_string "FuncDef" ; ()
  | declaration -> print_string "decl" ; ()
  | _ -> print_string "unknown type" ; ()  
  
*)


let rec iter_ast root = 
match root with
| _::tl -> iter_ast tl; ()
| [] -> print_string "done"; () 

let () =
let lexbuf = Lexing.from_channel stdin in
  try 
    let ast = Parser.program Lexer.lexer lexbuf in
    iter_ast ast;
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

