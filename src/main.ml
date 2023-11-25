open Lexing

let get_position lexbuf filename =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" filename
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) 


let compile filename =
  let inx = Core.In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  try 
    let ast = Parser.program Lexer.lexer lexbuf in
    Semantic.check_root ast ;
    exit 0
  with 
  | Parser.Error -> 
    let err_msg = Printf.sprintf "%s: %s\n" (get_position lexbuf filename) "Syntax error" in
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