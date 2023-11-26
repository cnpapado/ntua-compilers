open Ast
open Symbol
open Symbtest
open Identifier

exception SemError of string
exception InternalSemError of string

let check_all = List.map 

let rec check_root root = 
  match root with
  ParserAST.Root(x) -> 
    Printf.printf "root\n"; 
    initSymbolTable 256;
    openScope ();
    let sem_func_def = check_func_def x in
    printSymbolTable ();
    closeScope ();
    (* De-capsulate the record outside FuncDef for Root *)
    match sem_func_def with SemAST.FuncDef(y) -> SemAST.Root(y)


and check_func_def x =
  Printf.printf "func definition\n";  
  let sem_header = check_header x.func_def_header false in
  let sem_locals = check_all check_local_def x.func_def_local in
  let ret_type = sem_header.header_ret in
  let sem_block = check_block x.func_def_block ret_type in
  SemAST.FuncDef { func_def_header=sem_header; func_def_local=sem_locals; func_def_block=sem_block; meta={typ=None}}


and check_header h is_declaration =
  match h with 
  | ParserAST.Header(x) -> 
  Printf.printf "header\n";
  let fun_entry = newFunction (id_make x.header_id) true in (

  if is_declaration then forwardFunction fun_entry;

  let f fpar_tuple = 
    match fpar_tuple with (mode, id, typ) -> 
      newParameter (id_make id) typ mode fun_entry true 
  in
  ignore (check_all f x.header_fpar_defs);
  endFunctionHeader fun_entry x.header_ret);
  SemAST.Header {
    header_id=x.header_id;
    header_fpar_defs=x.header_fpar_defs; 
    header_ret=x.header_ret;
    meta={typ=None}
  }

and check_local_def x = 
  Printf.printf "local def ";
  match x with 
  | ParserAST.FuncDef(y)  -> check_func_def y
  | ParserAST.FuncDecl(y) -> check_func_decl y
  | ParserAST.VarDef(y)   -> check_var_def y 


and check_func_decl h =
  Printf.printf "func declaration\n";
  SemAST.FuncDecl(check_header h true)

and check_block b parent_ret_type = 
  match b with 
  | Block(stmt_list) ->
  Printf.printf "block\n";
  SemAST.Block (check_all (check_stmt parent_ret_type) stmt_list)

and check_stmt parent_ret_type single_stmt = 
  Printf.printf "stmt\n";
  match single_stmt with 
  | ParserAST.EmptyStmt -> SemAST.EmptyStmt
  | ParserAST.Assign({lvalue=l; rvalue=r_expr; meta=_}) ->
    
    let sem_l = check_lval l in (* find the type of l and save it to meta *)
    let sem_r = check_rval r_expr in (* find the type of r and save it to meta *) 
    (* check lvalue not an array*)
    if sem_l.meta.typ = Some TYPE_array{_; _} then 
      raise (SemError "Lvalue in assignment should not be an array")
    (* check both sides are of same type *)
    if sem_l.meta.typ <> sem_r.meta.typ then 
      raise (SemError "Type mismatch on assignment")
    
    SemAST.Assign({lvalue=sem_l; rvalue=sem_l; meta={typ=sem_l.meta.typ}}) (* fill typ with the type *)

  | ParserAST.Block(_) -> check_block parent_ret_type single_stmt
  | ParserAST.StmtFuncCall(f) -> 
    let sem_func_call = check_func_call f in
    if sem_func_call.meta.typ <> Some TYPE_nothing then (raise (SemError "stmt func call must be a procedure"))
    else SemAST.StmtFuncCall(sem_func_call)
  | ParserAST.If(i) -> 
    SemAST.If {
      if_cond = check_cond i.if_cond; 
      ifstmt = check_stmt i.ifstmt; 
      elsestmt = if i.elsestmt == None then None else check_stmt i.elsestmt; 
      meta = {typ=None} (* if and else parts should not have the same type right? *)
    }
  | ParserAST.While(w) ->
    SemAST.While {
      while_cond = check_cond w.while_cond;
      whilestmt = check_stmt w.whilestmt;
      meta = {typ=None}
    }
  | ParserAST.Return(e) ->
    let sem_e = 
    match e with 
    | None -> 
      if parent_ret_type <> Some TYPE_nothing then 
        (raise (SemError "this function returns nothing but is not a procedure")) 
      else None
    | Some ex ->
      let sem_e = check_expr ex in
      if sem_e.meta.typ <> parent_ret_type then (raise (SemError "type mismatch between return type and return value"))
      else sem_e in
    SemAST.Return(sem_e)

and rec check_lval l = 
  match l with 
  | ParserAST.LvalueId {id=lval_id; meta=_} -> 
    (* lookup type and save it to new meta *)
    let e = lookupEntry (id_make lval_id) LOOKUP_ALL_SCOPES true in (* all scopes ?? *)
    (* what happens if not found? 
       does id_make produce the same ids each time? *)
    let lval_typ = 
      match e.entry_info with 
      | ENTRY_variable {variable_type; _ } -> variable_type
      | ENTRY_parameter {parameter_type; _; _} -> parameter_type
      | _ -> raise (InternalSemError "found non var/param while looking up type of lval\n")
    in SemAST.LvalueId {id=lval_id; meta={typ = lval_typ}}
  
  | ParserAST.LvalueString {s=lval_str; meta=_} -> 
    SemAST.LvalueString {id=lval_str; meta={typ = Some TYPE_stringconst}} 

  | ParserAST.LvalueArr {arr=(lval_arr, idx_expr); meta=_} -> 
    (* represents an array element and thus has the value of it's element *)
    let sem_lval_arr = check_lval lval_arr in (* check and find type of element (lval_arr) turning it into SemAST *)
    let sem_idx_expr = check_expr idx_expr in (* same *)
    (* check that idx evaluates to int *)
    if sem_idx_expr.meta.typ <> Some TYPE_int then (raise (SemError "id of lval array not an int"))
    (* can you index with an char ? *)
    SemAST.LvalueArr {arr=(sem_lval_arr, sem_idx_expr); meta={typ = sem_lval_arr.meta.typ}} 


and check_func_call f = 
  match f with 
  | FuncCall(fcall) ->

    (* check the types, pass mode and number of actual and formal parameters are equal *)
    let actual_params = fcall.parameters
    
    (* find the corresponding symtable entry *)
    let e = lookupEntry fcall.name LOOKUP_ALL_SCOPES true in (* all scopes ?? *)
    (* what happens if not found? 
       does id_make produce the same ids each time? *)
    let (formal_params_entries, ret_typ) = match with
    | ENTRY_function inf -> (inf.function_paramlist, inf.function_result)
    | _ -> (raise (InternalSemError "found a non function entry when looking up a function")) in

    let get_type_and_mode e = 
      match e with
      | ENTRY_parameter inf -> (inf.parameter_type, inf.parameter_mode)
      | _ -> (raise (InternalSemError "found a non param entry in function's params")) in

    let formal_params = get_type_and_mode formal_params_entries in
    
    let check_params fp ap = 
      let sem_ap = check_expr ap in
      let (fp_typ, fp_mode) = fp in
      if fp_typ <> ap.meta.typ then (raise (SemError "formal and actual parameter have different type"))
      let is_lvalue = match sem_ap with
      | Lvalue _ -> true 
      | _ -> false
      else if (fp_mode == PASS_BY_REFERENCE && not is_lvalue) || (fp_mode == PASS_BY_VALUE && is_lvalue) then (raise (SemError "formal and actual parameter have different pass mode"))
      else sem_ap in

    let check_all_params f a = 
      match (f,a) with 
      | (fh::ftl, ah::atl) -> check_params dh ah :: check_all_params ftl atl (* tha xtyphsei? *)
      | ([],[]) -> []
      | _ -> (raise (SemError "different number of formal and actual parameters")) in
    
    let sem_actual_params = check_all_params formal_params actual_params in

    SemAST.FuncCall {name=fcall.name; parameters=sem_actual_params; meta={typ=ret_typ}}
    

and rec check_expr e = 
  match e with
  | ParserAST.Char {c=ch; meta=_} -> SemAST.Char {c=ch; meta={typ= Some TYPE_char}}
  (* | ParserAST.Id of {id=the_id; meta=_} -> 
    (* lookup and get type of id *)
    let e = lookupEntry fcall.name LOOKUP_ALL_SCOPES true in (* all scopes ?? *)
    let id_typ = 
    match with
    | ENTRY_variable inf -> inf.variable_type
    | _ -> (raise (InternalSemError "found a non variable entry when looking up an id")) in
    SemAST.Id {id=the_id; meta={typ:id_typ}}
  | ParserAST.String of {s=the_str; meta=_} -> ParserAST.String of {s=the_str; meta={typ:}} *)
  | ParserAST.Lvalue(lval) -> SemAST.Lvalue(check_lval lval)
  | ParserAST.ExprFuncCall(func_call) -> SemAST.ExprFuncCall(check_func_call func_call)
  | ParserAST.SignedExpr {sign; e; meta=_} -> 
    let sem_expr = check_expr the_expr in 
    if sem_expr.meta.typ <> Some TYPE_int then (raise (SemError "applying unary operation to non int"))
    else SemAST.SignedExpr({sign=sign; e=e; meta={typ=sem_expr.meta.typ}})
  | ParserAST.BinExpr {l; r; op; meta=_} ->
    let sem_l_expr = check_expr l in
    let sem_r_expr = check_expr r in
    if not (sem_l_expr.meta.typ = Some TYPE_int && sem_r_expr.meta.typ = Some TYPE_int) then
      raise (raise (SemError "applying binary arithmetic operation to non ints"))
    else SemAST.BinExpr {l=sem_l_expr; r=sem_r_expr; op=op; meta={typ=Some TYPE_int}}

and check_var_def x = 
  Printf.printf "var def\n";
  let f id = newVariable (id_make id) x.var_def_typ true in
  ignore (check_all f x.var_def_id);
  SemAST.VarDef {
    var_def_id=x.var_def_id;
    var_def_typ=x.var_def_typ;
    meta={typ=None}
  }