open Lexing

let exit_with_error err_msg = 
  Printf.fprintf stderr "\n%s\n" err_msg ;
  exit 1

let compile filename f_flag i_flag o_flag =
  let inx = 
    if !f_flag || !i_flag then (* read from stdin *)
      stdin
    else
      Core.In_channel.create filename 
  in
  let lexbuf = Lexing.from_channel inx in
  try 
    let ast = Grace.Parser.program Grace.Lexer.lexer lexbuf in
    let sem_ast = Grace.Semantic.check_root ast in
    (* let _ = Printf.printf "\n------------\nSEM AST\n------------\n %s" (Grace.Pretty_print.str_of_ast sem_ast) in  *)
    let llifted_ast = Grace.Llift.llift sem_ast in 
    let _ = Printf.printf "\n------------\nLLIFTED AST\n------------\n %s" (Grace.Pretty_print.str_of_ast llifted_ast) in 
    (* let _ = Printf.printf "\n------------\nLLVM IR\n------------\n %s" "" in  *)
    let _ = Grace.Codegen.emit_root llifted_ast in 
    let _ = Llvm_analysis.verify_module Grace.Codegen.the_module in 
    
    if !o_flag then
      (* let _ = Printf.printf "\n------------\nopt IR\n------------\n %s" "" in  *)
      begin
        let pm = Llvm.PassManager.create () in
        Llvm_scalar_opts.add_memcpy_opt pm;
        Llvm_scalar_opts.add_memory_to_register_promotion pm;
        (* Llvm_scalar_opts.add_constant_propagation pm; *)
        Llvm_scalar_opts.add_instruction_combination pm;
        Llvm_scalar_opts.add_reassociation pm;
        Llvm_scalar_opts.add_cfg_simplification pm;
        Llvm_scalar_opts.add_gvn pm;
        Llvm_scalar_opts.add_aggressive_dce pm;
        Llvm_ipo.add_function_inlining pm;
        ignore @@ Llvm.PassManager.run_module Grace.Codegen.the_module pm
      end;

    if !i_flag then
      begin
        print_endline @@ Llvm.string_of_llmodule Grace.Codegen.the_module;
        exit 0 
      end
    else if not !i_flag then
      begin
        let target_machine =
          Llvm_all_backends.initialize ();
          let default_triple = Llvm_target.Target.default_triple () in
          let target = Llvm_target.Target.by_triple default_triple in
          Llvm_target.TargetMachine.create ~triple:default_triple target
        in
        
        let asm_filetype = Llvm_target.CodeGenFileType.AssemblyFile in

        if !f_flag then
          let memory_buffer =
            Llvm_target.TargetMachine.emit_to_memory_buffer Grace.Codegen.the_module asm_filetype target_machine
          in
          print_endline @@ Llvm.MemoryBuffer.as_string memory_buffer; 
          exit 0
        else begin
          let fname = Filename.remove_extension (Filename.basename filename) (* Filename.remove_extension filename *)
          in
          Llvm.print_module (fname ^ ".imm") Grace.Codegen.the_module;
          Llvm_target.TargetMachine.emit_to_file Grace.Codegen.the_module asm_filetype (fname ^ ".asm") target_machine;
          let cmd = Printf.sprintf "clang %s ../runtime_lib/lib.a -o %s -no-pie" (fname ^ ".asm") (fname ^ ".exe") in
          let status = Sys.command cmd in
          if status != 0
          then begin
            Printf.printf "Failed to link object file with library. Clang is required for this. Maybe it needs to be installed.\n";
            exit 1
          end
          else exit 0
        end
      end
  with 
  | Grace.Lexer.LexicalError msg ->
    let get_position lexbuf filename =
      let pos = lexbuf.lex_curr_p in
        Printf.sprintf "%s:%d:%d" filename
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) in
      let err_msg = Printf.sprintf "%s: Lexical error: %s\n" (get_position lexbuf filename) msg in
      exit_with_error err_msg 
  | Grace.Parser.Error -> 
    let get_position lexbuf filename =
    let pos = lexbuf.lex_curr_p in
      Printf.sprintf "%s:%d:%d" filename
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) in
    let err_msg = Printf.sprintf "%s: Syntax error\n" (get_position lexbuf filename) in
    exit_with_error err_msg
  | Grace.Semantic.SemError (sem_msg, loc) ->
    let pos = loc in
      let line_no = pos.pos_lnum in
      let col_no = (pos.pos_cnum - pos.pos_bol + 1) in
    let err_msg = Printf.sprintf "%s:%d:%d: Semantic error: %s\n" filename line_no col_no sem_msg in
    exit_with_error err_msg
  | Grace.Error.SymTableException msg ->
    let err_msg = Printf.sprintf "%s: Semantic Error: %s\n" filename msg in
    exit_with_error err_msg
  (* | _ -> exit_with_error "Error while compiling program\n" *)
    
    Core.In_channel.close inx
    
    

let usage_msg = "./gracec [--verbose] {<file>|-i|-f} [-O] "
let verbose = ref false
let o_flag = ref false
let f_flag = ref false
let i_flag = ref false
let input_file = ref ""

let anon_fun filename =
  input_file := filename

let speclist =
  [("--verbose", Arg.Set verbose, "Output debug information");
  ("-O", Arg.Set o_flag, "Enable optimizations");
  ("-f", Arg.Set f_flag, "Read src code from stdin and write assembly into stdout");
  ("-i", Arg.Set i_flag, "Read src code from stdin and write IR into stdout")
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;
  try 
    compile !input_file f_flag i_flag o_flag
  with Sys_error msg -> exit_with_error msg