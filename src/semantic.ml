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
  let sem_block = check_block x.func_def_block in
  SemAST.FuncDef { func_def_header=sem_header; func_def_local=sem_locals; func_def_block=sem_block; meta={typ=42}}


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
    meta={typ=42}
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

and check_block b = 
  match b with 
  | Block(stmt_list) ->
  Printf.printf "block\n";
  SemAST.Block (check_all check_stmt stmt_list)

and check_stmt single_stmt = 
  Printf.printf "stmt\n";
  match single_stmt with 
  | ParserAST.EmptyStmt -> SemAST.EmptyStmt
  | ParserAST.Assign({lvalue=l; rvalue=r_expr; meta=_}) ->
    
    let sem_l = check_lval l in (* find the type of l and save it to meta *)
    let sem_r = check_rval r_expr in (* find the type of r and save it to meta *) 
    (* check lvalue not an array*)
    if sem_l.meta.typ == TYPE_array{ttype=_; size=_} then 
      raise (SemError "Lvalue in assignment should not be an array")
    (* check both sides are of same type *)
    if sem_l.meta.typ <> sem_r.meta.typ then 
      raise (SemError "Type mismatch on assignment")
    
    SemAST.Assign({lvalue=sem_l; rvalue=sem_l; meta={typ=42}}) (* fill typ with the type *)

  | ParserAST.Block(_) -> check_block single_stmt
  | ParserAST.StmtFuncCall of func_call
  | ParserAST.If of if_expr
  | ParserAST.While of while_expr
  | ParserAST.Return of expr option


and rec check_lval l = 
  match l with 
  | ParserAST.LvalueId {id=lval_id; meta=_} -> 
    (* lookup type and save it to new meta *)
    let e = lookupEntry lval_id LOOKUP_ALL_SCOPES true in (* all scopes ?? *)
    let lval_typ = 
      match e.entry_info with 
      | ENTRY_variable {variable_type=typ; variable_offset=_ } -> typ
      | ENTRY_parameter {parameter_type=typ; parameter_offset=_; parameter_mode=_} -> typ
      | _ -> raise (InternalSemError "found non var/param while looking up type of lval\n")
    in SemAST.LvalueId {id=lval_id; meta={typ = lval_typ}}
  
  | ParserAST.LvalueString {s=lval_str; meta=_} -> 
    SemAST.LvalueString {id=lval_str; meta={typ = TYPE_stringconst}} 

  | ParserAST.LvalueArr {arr=(lval_arr, idx_expr); meta=_} -> 
    (* represents an array element and thus has the value of it's element *)
    let sem_lval_arr = check_lval lval_arr in (* check and find type of element (lval_arr) turning it into SemAST *)
    let sem_idx_expr = ... in (* check etc *)
    (* check that idx evaluates to int *)
    if sem_idx_expr.meta.typ <> TYPE_int then (raise (SemError "id of lval array not an int"))
    (* can you index with an char ? *)
    SemAST.LvalueArr {arr=(sem_lval_arr, sem_idx_expr); meta={typ = sem_lval_arr.meta.typ}} 


and check_func_call f = 
  match f with 
  | FuncCall(fcall) ->
    (* check the number of actual parameters equals the number of formal parameters *)

    (* check the types of actual parameters are the same with the types of formal parameters *)

    (* check the pass mode of actual parameters are the same with the pass mode of formal parameters *)



and check_var_def x = 
  Printf.printf "var def\n";
  let f id = newVariable (id_make id) x.var_def_typ true in
  ignore (check_all f x.var_def_id);
  SemAST.VarDef {
    var_def_id=x.var_def_id;
    var_def_typ=x.var_def_typ;
    meta={typ=42}
  }