open Lexing

let compile filename =
  let inx = Core.In_channel.create filename in (* catch unknown file error *)
  let lexbuf = Lexing.from_channel inx in
  try 
    let ast = Grace.Parser.program Grace.Lexer.lexer lexbuf in
    let sem_ast = Grace.Semantic.check_root ast in
    let _ = Printf.printf "\n------------\nSEM AST\n------------\n %s" (Grace.Pretty_print.str_of_ast sem_ast) in 
    let llifted_ast = Grace.Llift.llift sem_ast in 
    let _ = Printf.printf "\n------------\nLLIFTED AST\n------------\n %s" (Grace.Pretty_print.str_of_ast llifted_ast) in 
    let _ = Printf.printf "\n------------\nLLVM IR\n------------\n %s" "" in 
    let the_module = Grace.Codegen.emit_root llifted_ast in (* ?? *)
    let verification = Llvm_analysis.verify_module Grace.Codegen.the_module in 
    print_endline @@ Llvm.string_of_llmodule Grace.Codegen.the_module;

    let target_machine =
      Llvm_all_backends.initialize ();
      let default_triple = Llvm_target.Target.default_triple () in
      let target = Llvm_target.Target.by_triple default_triple in
      Llvm_target.TargetMachine.create ~triple:default_triple target
    in
    
    let asm_filetype = Llvm_target.CodeGenFileType.AssemblyFile in

    (* let memory_buffer =
      Llvm_target.TargetMachine.emit_to_memory_buffer Codegen.the_module asm_filetype target_machine
    in
    print_endline @@ Llvm.MemoryBuffer.as_string memory_buffer; *)
    
    let fname = Filename.remove_extension filename
      (* match List.rev (String.split_on_char '.' filename) with
      | _::name::_ -> Printf.printf "%s\n" name; name
      | [] ->
        failwith "This should be unreachable. Splitting filename on '.' and getting empty list" *)
    in
    Llvm.print_module (fname ^ ".imm") Grace.Codegen.the_module;
    Llvm_target.TargetMachine.emit_to_file Grace.Codegen.the_module asm_filetype (fname ^ ".asm") target_machine;
    let cmd = Printf.sprintf "clang %s ../runtime_lib/lib.a -o %s" (fname ^ ".asm") fname in
    (* let cmd = Printf.sprintf "clang %s -o %s" (fname ^ ".asm") fname in *)
    let _ = Sys.command "ls ../runtime_lib" in
    let status = Sys.command cmd in
    if status != 0
    then
      print_endline "Failed to link object file with library. \
                     Clang is required for this. Maybe it needs to be installed.";
      exit 6


    exit 0
  with 
  | Grace.Lexer.LexicalError msg ->
    let get_position lexbuf filename =
      let pos = lexbuf.lex_curr_p in
        Printf.sprintf "%s:%d:%d" filename
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) in
      let err_msg = Printf.sprintf "%s: Lexical error: %s\n" (get_position lexbuf filename) msg in
      Printf.fprintf stderr "\n%s\n" err_msg ;
      exit (-1)
  | Grace.Parser.Error -> 
    let get_position lexbuf filename =
    let pos = lexbuf.lex_curr_p in
      Printf.sprintf "%s:%d:%d" filename
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) in
    let err_msg = Printf.sprintf "%s: Syntax error\n" (get_position lexbuf filename) in
    Printf.fprintf stderr "\n%s\n" err_msg ;
    exit (-1)
  | Grace.Semantic.SemError (sem_msg, loc) ->
    let pos = loc in
      let line_no = pos.pos_lnum in
      let col_no = (pos.pos_cnum - pos.pos_bol + 1) in
    let err_msg = Printf.sprintf "%s:%d:%d: Semantic error: %s\n" filename line_no col_no sem_msg in
    Printf.fprintf stderr "\n%s\n" err_msg ;
    exit (-1)
  | Grace.Error.SymTableException msg ->
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