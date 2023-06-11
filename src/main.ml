(* open Lexer
open Parser

let string_of_token token =
  match token with
    | T_and           -> "T_and"
    | T_char          -> "T_char"
    | T_div           -> "T_div"
    | T_do            -> "T_do"
    | T_else          -> "T_else"
    | T_fun           -> "T_fun"
    | T_if            -> "T_if"
    | T_int           -> "T_int"
    | T_mod           -> "T_mod"
    | T_not           -> "T_not"
    | T_nothing       -> "T_nothing"
    | T_or            -> "T_or"
    | T_ref           -> "T_ref"
    | T_return        -> "T_return"
    | T_then          -> "T_then"
    | T_var           -> "T_var"
    | T_while         -> "T_while"
    | T_plus          -> "T_plus"     
    | T_minus         -> "T_minus"  
    | T_times         -> "T_times"
    | T_eq            -> "T_eq"  
    | T_neq           -> "T_neq"  
    | T_assign        -> "T_assign"  
    | T_gt            -> "T_gt"      
    | T_lt            -> "T_lt"       
    | T_ge            -> "T_ge"       
    | T_le            -> "T_le"  
    | T_semicol       -> "T_semicol"           
    | T_lparen        -> "T_lparen"          
    | T_rparen        -> "T_rparen"          
    | T_lbracket      -> "T_lbracket"            
    | T_rbracket      -> "T_rbracket"            
    | T_lcurl         -> "T_lcurl"         
    | T_rcurl         -> "T_rcurl"   
    | T_colon         -> "T_colon"         
    | T_comma         -> "T_comma" 
    | T_id(_)            -> "T_id"
    | T_charconst(_)     -> "T_charconst"
    | T_stringliteral(_) -> "T_stringliteral"
    | T_intconst(_)      -> "T_intconst"
    | T_eof -> "T_eof"


let main =
  let lexbuf = Lexing.from_channel stdin in
  let rec loop () =
    let token = lexer lexbuf in
    Printf.printf "token=%s, lexeme=\"%s\"\n"
      (string_of_token token) (Lexing.lexeme lexbuf);
    if token <> T_eof then loop () in
  loop () *)



open Parser
open Lexing
open Core

let get_position lexbuf filename =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" filename
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) 


    let compile filename =
      let inx = In_channel.create filename in
      let lexbuf = Lexing.from_channel inx in
      try 
        let ast = Parser.program Lexer.lexer lexbuf in
        exit 0
      with 
      | _ -> 
        let err_msg = Printf.sprintf "%s: %s\n" (get_position lexbuf filename) "error" in
        Printf.fprintf stderr "\n%s\n" err_msg ;
        exit (-1)
        
      In_channel.close inx
    
    
    let () =
      Command.basic ~summary:"compile grace src file"
        Command.Param.(
          anon ("filename" %: string)
          |> map ~f:(fun filename ->
            fun () ->
              compile filename))
      |> Command_unix.run


(* 
open Parser
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)



let main =
  let lexbuf = Lexing.from_channel stdin in
  try
    Parser.program Lexer.lexer lexbuf;
    exit 0
  with _ ->
    (* Printf.eprintf "syntax error\n"; *)
    Printf.fprintf stderr "syntax error %a\n" print_position lexbuf;
    exit 1 *)