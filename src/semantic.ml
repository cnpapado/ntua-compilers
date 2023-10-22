open Ast
open Symbol
open Symbtest

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
  check_header x.func_def_header;
  ignore (check_all check_local_def x.func_def_local);
  check_block x.func_def_block


and check_header x =
  Printf.printf "header\n"

and check_local_def x = 
  Printf.printf "local def ";
  match x with 
  | FuncDef(y) -> 
    Printf.printf "func def\n";
    check_func_def y
  | FuncDecl(y) -> 
    Printf.printf "func decl\n";
    check_func_decl y
  | VarDef(_) -> Printf.printf "var def\n"


and check_func_decl x = 
  Printf.printf "func declaration\n";
  check_header x.func_decl_header

and check_block x = 
  Printf.printf "block\n"