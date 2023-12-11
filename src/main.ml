open Lexing

let compile filename =
  let inx = Core.In_channel.create filename in (* catch unknown file error *)
  let lexbuf = Lexing.from_channel inx in
  try 
    let ast = Parser.program Lexer.lexer lexbuf in
    ignore (Semantic.check_root ast) ;
    exit 0
  with 
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
    
    Core.In_channel.close inx
    
    
let () =
  Command.basic ~summary:"compile grace src file"
    Command.Param.(
      anon ("filename" %: string)
      |> map ~f:(fun filename ->
        fun () ->
          compile filename))
  |> Command_unix.run