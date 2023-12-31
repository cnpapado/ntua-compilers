open Ast


(* return the same AST but with all function declarations, definitions and calls
 * renamed by prepending the prefix *)
let rec rename_uniq prefix (SemAST.FuncDef def) =
  let fname = function SemAST.Header h -> h.header_id in
  let current_fname = fname def.func_def_header in
  let rename_header outer_fun_name fun_header = match fun_header with 
    | SemAST.Header h -> SemAST.Header{h with header_id=prefix^outer_fun_name} 
  in
  let rename_def d = match d with 
    | SemAST.FuncDef _ -> rename_uniq (prefix ^ current_fname ^ "_") d
    | SemAST.FuncDecl h -> SemAST.FuncDecl (rename_header (fname h) h)
    | SemAST.VarDef _ -> d
  in
  SemAST.FuncDef {
    def with 
      func_def_header=rename_header current_fname def.func_def_header; 
      func_def_local=List.map rename_def def.func_def_local
    }

let rename_ast = rename_uniq "" 