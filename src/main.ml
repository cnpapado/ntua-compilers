open Lexing

let compile filename =
  let inx = Core.In_channel.create filename in (* catch unknown file error *)
  let lexbuf = Lexing.from_channel inx in
  try 
    let ast = Parser.program Lexer.lexer lexbuf in
    let sem_ast = Semantic.check_root ast in
    let _ = Printf.printf "\n\n\n\n %s" (Pretty_print.str_of_ast sem_ast) in 
    let llifted_ast = Llift.llift sem_ast in 
    let _ = Printf.printf "\n\n\n\n %s" (Pretty_print.str_of_ast llifted_ast) in 
    (* let the_module = Codegen.emit_root sem_ast in *)
    (* let verification = Llvm_analysis.verify_module Codegen.the_module in  *)
    (* print_endline @@ Llvm.string_of_llmodule Codegen.the_module; *)
    exit 0
  with 
  | Lexer.LexicalError msg ->
    let get_position lexbuf filename =
      let pos = lexbuf.lex_curr_p in
        Printf.sprintf "%s:%d:%d" filename
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) in
      let err_msg = Printf.sprintf "%s: Lexical error: %s\n" (get_position lexbuf filename) msg in
      Printf.fprintf stderr "\n%s\n" err_msg ;
      exit (-1)
  | Parser.Error -> 
    let get_position lexbuf filename =
    let pos = lexbuf.lex_curr_p in
      Printf.sprintf "%s:%d:%d" filename
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) in
    let err_msg = Printf.sprintf "%s: Syntax error\n" (get_position lexbuf filename) in
    Printf.fprintf stderr "\n%s\n" err_msg ;
    exit (-1)
  | Semantic.SemError (sem_msg, loc) ->
    let pos = loc in
      let line_no = pos.pos_lnum in
      let col_no = (pos.pos_cnum - pos.pos_bol + 1) in
    let err_msg = Printf.sprintf "%s:%d:%d: Semantic error: %s\n" filename line_no col_no sem_msg in
    Printf.fprintf stderr "\n%s\n" err_msg ;
    exit (-1)
  | Error.SymTableException msg ->
    let err_msg = Printf.sprintf "%s: Semantic Error: %s\n" filename msg in (* maybe add location here *)
    Printf.fprintf stderr "\n%s\n" err_msg ;
    exit (-1)
    
    Core.In_channel.close inx
    
    
let () =
  Command.basic ~summary:"compile grace src file"
    Command.Param.(
      anon ("filename" %: string)
      |> map ~f:(fun filename ->
        fun () ->
          compile filename))
  |> Command_unix.run