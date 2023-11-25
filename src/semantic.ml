open Ast.ParserAST
open Symbol
open Symbtest
open Identifier

let check_all = List.map 

let rec check_root root = 
  match root with
  Root(x) -> 
    Printf.printf "root\n"; 
    initSymbolTable 256;
    openScope ();
    check_func_def x;
    printSymbolTable ();
    closeScope ();
    ()


and check_func_def x =
  Printf.printf "func definition\n";  
  check_header x.func_def_header false;
  ignore (check_all check_local_def x.func_def_local);
  check_block x.func_def_block


and check_header x is_declaration =
  Printf.printf "header\n";
  let fun_entry = newFunction (id_make x.header_id) true in (

  if is_declaration then forwardFunction fun_entry;

  let f fpar_tuple = 
    match fpar_tuple with (mode, id, typ) -> 
      newParameter (id_make id) typ mode fun_entry true 
  in
  ignore (List.map f x.header_fpar_defs); 

  endFunctionHeader fun_entry x.header_ret);


and check_local_def x = 
  Printf.printf "local def ";
  match x with 
  | FuncDef(y)  -> check_func_def y
  | FuncDecl(y) -> check_func_decl y
  | VarDef(y)   -> check_var_def y


and check_func_decl x = 
  Printf.printf "func declaration\n";
  check_header x.func_decl_header true

and check_block x = 
  Printf.printf "block\n"

and check_var_def x = 
  Printf.printf "var def\n";
  let f id = newVariable (id_make id) x.var_def_ret true in
  ignore (List.map f x.var_def_id);