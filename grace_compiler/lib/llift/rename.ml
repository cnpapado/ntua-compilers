open Ast
open Symbol

module SS = Set.Make(String)
let function_names = ref SS.empty

let add_mapping ~newid ~oldid is_decl = 
  let fun_entry = newFunction (Identifier.id_make oldid) true in 
  Printf.printf "\n\nMAPPING %s --> %s\nset:" oldid newid;
  let _ = 
    match fun_entry.entry_info with
      | ENTRY_function inf -> inf.function_newName <- newid in
    
    (* add to set with the function ids *)
    function_names := SS.add newid !function_names;
    (* SS.iter print_endline !function_names; *)
    
    if is_decl then forwardFunction fun_entry;
    endFunctionHeader fun_entry TYPE_uninitialized (* we don't care about ret typ here *) 


(* return the same AST but with all function declarations, definitions and calls
 * renamed by prepending the prefix *)
let rec rename_uniq prefix (SemAST.FuncDef def) =
  let fname = function SemAST.Header h -> h.header_id in
  let current_fname = fname def.func_def_header in
  Printf.printf "\n\nrenaming %s" current_fname;
  let rename_header prefix fun_name fun_header is_decl = 
    Printf.printf "\n\nrenaming header %s prefix: %s" fun_name prefix;
    let newid = prefix^fun_name in
    add_mapping ~newid:newid ~oldid:fun_name is_decl;
    openScope ();
    match fun_header with 
    | SemAST.Header h -> SemAST.Header{h with header_id=newid} 
  in
  let rec rename_calls (SemAST.Block stmt_list) =
    Printf.printf "\n\nrenaming calls";
    let rename_call (SemAST.FuncCall f) = 
      (* lookup and rename with new name from symb table *)
      let f_entry = lookupEntry (Identifier.id_make f.name) LOOKUP_ALL_SCOPES true in
      let newid = match f_entry.entry_info with ENTRY_function inf -> inf.function_newName in
      Printf.printf "\n\nRENAMINFG %s --> %s\n" f.name newid;
      SemAST.FuncCall {f with name=newid}
    in
    let rec rename_expr e = match e with 
      | SemAST.Int i -> SemAST.Int i
      | SemAST.Char c -> SemAST.Char c
      | SemAST.Lvalue lval -> SemAST.Lvalue 
        begin 
          match lval with 
          | SemAST.LvalueArr {arr=(lval_arr, idx_expr); meta} -> SemAST.LvalueArr {arr=(lval_arr, rename_expr idx_expr); meta=meta}
          | _ -> lval 
        end
      | SemAST.ExprFuncCall f -> SemAST.ExprFuncCall (rename_call f) 
      | SemAST.SignedExpr se -> SemAST.SignedExpr {se with e=rename_expr se.e}
      | SemAST.BinExpr b -> SemAST.BinExpr {b with l=rename_expr b.l; r=rename_expr b.r}
    and rename_cond c = match c with
      | SemAST.ExprCond c -> SemAST.ExprCond {c with l=rename_expr c.l; r=rename_expr c.r}
      | SemAST.CompoundCond c -> SemAST.CompoundCond {c with l=rename_cond c.l; r=rename_cond c.r}
      | SemAST.NegatedCond c -> SemAST.NegatedCond (rename_cond c)
    and rename_stmt stmt = match stmt with 
      | SemAST.EmptyStmt -> SemAST.EmptyStmt
      | SemAST.Assign assign -> SemAST.Assign {assign with rvalue=rename_expr assign.rvalue} 
      | SemAST.Block _ -> rename_calls stmt
      | SemAST.StmtFuncCall f -> SemAST.StmtFuncCall (rename_call f) 
      | SemAST.If i -> SemAST.If {
        i with 
        if_cond=rename_cond i.if_cond; 
        ifstmt=rename_stmt i.ifstmt; 
        elsestmt=begin match i.elsestmt with Some e -> Some (rename_stmt e) | None -> None end
        }
      | SemAST.While w -> SemAST.While {
        w with 
        while_cond=rename_cond w.while_cond;
        whilestmt=rename_stmt w.whilestmt;
        }
      | SemAST.Return r -> SemAST.Return {
        ret=begin match r.ret with 
        | Some sr -> Some (rename_expr sr) 
        | None -> None end; 
        meta=r.meta}
    in
      openScope ();
      let b = SemAST.Block (List.map rename_stmt stmt_list) in
      closeScope (); b
  in
  let rename_def d = 
    Printf.printf "\n\nrenaming def %s" "";
    let new_prefix = (prefix ^ current_fname ^ "_") in
    match d with 
    | SemAST.FuncDef this -> 
      let renamed_defs = rename_uniq new_prefix d in (* the current funcdef with child defs and decls renamed *)
      let renamed_defs_callsites = SemAST.FuncDef {
        begin match renamed_defs with SemAST.FuncDef f -> f end 
        with func_def_block=rename_calls this.func_def_block} in (* the current funcdef with calls renamed too *)
      closeScope (); renamed_defs_callsites
    | SemAST.FuncDecl h -> 
      let f = SemAST.FuncDecl (rename_header new_prefix (fname h) h true) in
      closeScope (); f
    | SemAST.VarDef _ -> d
  in
  let new_header = rename_header prefix current_fname def.func_def_header false in
  let new_defs = List.map rename_def def.func_def_local in
  let new_block = rename_calls def.func_def_block in
  SemAST.FuncDef {
    def with 
      func_def_header=new_header; 
      func_def_local=new_defs;
      func_def_block=new_block 
    }

let add_buildins_mappings () = 
  add_mapping ~newid:"writeInteger" ~oldid:"writeInteger" false;
  add_mapping ~newid:"writeChar" ~oldid:"writeChar" false;
  add_mapping ~newid:"writeString" ~oldid:"writeString" false;
  add_mapping ~newid:"readInteger" ~oldid:"readInteger" false;
  add_mapping ~newid:"readChar" ~oldid:"readChar" false;
  add_mapping ~newid:"readString" ~oldid:"readString" false;
  add_mapping ~newid:"ascii" ~oldid:"ascii" false;
  add_mapping ~newid:"aschrcii" ~oldid:"aschrcii" false;
  add_mapping ~newid:"strlen" ~oldid:"strlen" false;
  add_mapping ~newid:"strcmp" ~oldid:"strcmp" false;
  add_mapping ~newid:"strcpy" ~oldid:"strcpy" false;
  add_mapping ~newid:"strcat" ~oldid:"strcat" false


let rename_ast root = 
  initSymbolTable 256; 
  openScope ();
  add_buildins_mappings (); 
  let ast = rename_uniq "" root in
  closeScope (); 
  ast