open Ast
open Symbol

let add_mapping ~newid ~oldid is_decl = 
  let fun_entry = newFunction (Identifier.id_make oldid) true in 
  let _ = 
    match fun_entry.entry_info with
      | ENTRY_function inf -> inf.function_newName = newid in
    if is_decl then forwardFunction fun_entry;
    endFunctionHeader fun_entry TYPE_uninitialized (* we don't care about ret typ here *) 


(* return the same AST but with all function declarations, definitions and calls
 * renamed by prepending the prefix *)
let rec rename_uniq prefix (SemAST.FuncDef def) =
  let fname = function SemAST.Header h -> h.header_id in
  let current_fname = fname def.func_def_header in
  let rename_header fun_name fun_header is_decl = 
    let newid = prefix^fun_name in
    add_mapping ~newid:newid ~oldid:fun_name is_decl;
    openScope ();
    match fun_header with 
    | SemAST.Header h -> SemAST.Header{h with header_id=newid} 
  in
  let rename_calls prefix (SemAST.Block stmt_list) = 
    let rename_call (SemAST.FuncCall f) = 
      (* lookup and rename with new name from symb table *)
      let f_entry = lookupEntry (id_make f.name) LOOKUP_ALL_SCOPES true in
      let newid = match f_entry with ENTRY_function inf -> inf.function_newName in
      SemAST.StmtFuncCall {f with name=newid}  
    
    let rename_cond c = function
      | SemAST.ExprCond c -> SemAST.ExprCond {c with l=rename_expr c.l; r=rename_expr c.r}
      | SemAST.CompoundCond c -> SemAST.CompoundCond {c with l=rename_cond c.l; r=rename_cond c.r}
      | SemAST.NegatedCond c -> SemAST.NegatedCond (rename_cond c)
    and rename_expr e = function 
      | Int _ -> e
      | Char _ -> e
      | Lvalue lval -> begin match lval with 
        | SemAST.LvalueArr {arr=(lval_arr, idx_expr); meta} -> SemAST.LvalueArr {arr=(lval_arr, rename_expr idx_expr); meta=meta}
        | _ -> lval 
        end 
      | ExprFuncCall f -> rename_call f 
      | SignedExpr se -> SemAST.SignedExpr {se with e=rename_expr se.e}
      | BinExpr b -> SemAST.BinExpr {b with l=rename_expr b.l; r=rename_expr b.r}
    
    let rec rename_stmt stmt = function 
      | SemAST.Assign assign -> SemAST.Assign {assign with rvalue=rename_expr assign.rvalue} 
      | SemAST.Block _ -> rename_calls prefix stmt
      | SemAST.StmtFuncCall f -> rename_call f 
      | SemAST.If i -> SemAST.If {
        i with 
        if_cond=rename_cond i.if_cond; 
        ifstmt=rename_stmt i.ifstmt; 
        elsestmt=match i.elsestmt with Some e -> rename_stmt e;
        }
      | SemAST.While w -> SemAST.While {
        w with 
        while_cond=rename_cond w.while_cond;
        whilestmt=rename_stmt w.whilestmt;
        }
      | SemAST.Return r -> {ret=match r.ret with Some _ -> rename_expr r.ret | None -> None ; meta=r.meta}
  let rename_def d = 
    match d with 
    | SemAST.FuncDef this -> 
      let new_prefix = (prefix ^ current_fname ^ "_") in
      let renamed_defs = rename_uniq new_prefix d in (* the current funcdef with child defs and decls renamed *)
      let renamed_defs_callsites = SemAST.FuncDef {
        begin match renamed_defs with SemAST.FuncDef f -> f end 
        with func_def_block=rename_calls new_prefix this.func_def_block} in (* the current funcdef with calls renamed too *)
      closeScope (); renamed_defs_callsites
    | SemAST.FuncDecl h -> 
      let f = SemAST.FuncDecl (rename_header (fname h) h true) in
      closeScope (); f
    | SemAST.VarDef _ -> d
  in
  SemAST.FuncDef {
    def with 
      func_def_header=rename_header current_fname def.func_def_header false; 
      func_def_local=List.map rename_def def.func_def_local;
      func_def_block=rename_calls prefix def.func_def_block 
    }


(* todo: openScope, closeScope on blocks *)

let rename_ast = 
  let _ = initSymbolTable 256 in
  rename_uniq "" 