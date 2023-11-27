open Ast
open Symbol
open Symbtest
open Identifier
open Types

exception SemError of string
exception InternalSemError of string

let check_all = List.map 

let rec get_type4 c =
  match c with
  | SemAST.ExprCond i -> i.meta.typ
  | SemAST.CompoundCond i -> i.meta.typ 
  | SemAST.NegatedCond i -> get_type4 i
let get_type2 l =
  match l with
  | SemAST.LvalueId i -> i.meta.typ | SemAST.LvalueString i -> i.meta.typ | SemAST.LvalueArr i -> i.meta.typ
let get_type3 f = 
  match f with SemAST.FuncCall i -> i.meta.typ
let get_type e = 
  match e with 
  | SemAST.Int i -> i.meta.typ
  | SemAST.Char c -> c.meta.typ
  | SemAST.Lvalue lval -> get_type2 lval
  | SemAST.ExprFuncCall f -> get_type3 f 
  | SemAST.SignedExpr i -> i.meta.typ
  | SemAST.BinExpr i -> i.meta.typ

(* transform some simpler ParserAST nodes (which are not being typechecked) to SemAST nodes*)  
let to_sem_comp n =
  match n with
  | ParserAST.Lt -> SemAST.Lt 
  | ParserAST.Gt -> SemAST.Gt 
  | ParserAST.Le -> SemAST.Le 
  | ParserAST.Ge -> SemAST.Ge 
  | ParserAST.Eq -> SemAST.Eq 
  | ParserAST.Neq -> SemAST.Neq 
let to_sem_logical n =
  match n with 
  | ParserAST.And -> SemAST.And 
  | ParserAST.Or -> SemAST.Or
let to_sem_arithm n = 
  match n with 
  | ParserAST.Times -> SemAST.Times 
  | ParserAST.Div -> SemAST.Div 
  | ParserAST.Mod -> SemAST.Mod 
  | ParserAST.Plus -> SemAST.Plus 
  | ParserAST.Minus -> SemAST.Minus
let to_sem_uop n =
  match n with
  | ParserAST.UPlus -> SemAST.UPlus 
  | ParserAST.UMinus -> SemAST.UMinus

let check_header h is_declaration =
  match h with 
  | ParserAST.Header(x) -> 
  Printf.printf "header\n";
  let fun_entry = newFunction (id_make x.header_id) true in (

  if is_declaration then forwardFunction fun_entry;

  let f fpar_tuple = 
    match fpar_tuple with (mode, id, typ) -> 
      newParameter (id_make id) typ mode fun_entry true 
  in
  ignore (List.map f x.header_fpar_defs);
  endFunctionHeader fun_entry x.header_ret);
  SemAST.Header {
    header_id=x.header_id;
    header_fpar_defs=x.header_fpar_defs; 
    header_ret=x.header_ret;
    meta={typ=None}
  }

let check_func_decl h =
  Printf.printf "func declaration\n";
  SemAST.FuncDecl(check_header h true)


let check_var_def v = 
  match v with 
  | ParserAST.VarDef(x) ->
  Printf.printf "var def\n";
  let f id = newVariable (id_make id) x.var_def_typ true in
  ignore (check_all f x.var_def_id);
  SemAST.VarDef {
    var_def_id=x.var_def_id;
    var_def_typ=x.var_def_typ;
    meta={typ=None}
  }

let rec check_expr e = 
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
  | ParserAST.SignedExpr {sign=s; e; meta=_} -> 
    let sem_expr = check_expr e in 
    if get_type sem_expr <> Some TYPE_int then (raise (SemError "applying unary operation to non int"))
    else SemAST.SignedExpr({sign=to_sem_uop s; e=sem_expr; meta={typ=get_type sem_expr}})
  | ParserAST.BinExpr {l; r; op; meta=_} ->
    let sem_l_expr = check_expr l in
    let sem_r_expr = check_expr r in
    if not (get_type sem_l_expr = Some TYPE_int && get_type sem_r_expr = Some TYPE_int) then
      raise (raise (SemError "applying binary arithmetic operation to non ints"))
    else SemAST.BinExpr {l=sem_l_expr; r=sem_r_expr; op=to_sem_arithm op; meta={typ=Some TYPE_int}}
  
and check_lval l = 
  match l with 
  | ParserAST.LvalueId {id=lval_id; meta=_} -> 
    (* lookup type and save it to new meta *)
    let e = lookupEntry (id_make lval_id) LOOKUP_ALL_SCOPES true in (* all scopes ?? *)
    (* what happens if not found? 
        does id_make produce the same ids each time? *)
    let lval_typ = 
      match e.entry_info with 
      | ENTRY_variable {variable_type; variable_offset= _ } -> variable_type
      | ENTRY_parameter {parameter_type; parameter_offset=_; parameter_mode=_} -> parameter_type
      | _ -> raise (InternalSemError "found non var/param while looking up type of lval\n")
    in SemAST.LvalueId {id=lval_id; meta={typ = Some lval_typ}}
  
  | ParserAST.LvalueString {s=lval_str; meta=_} -> 
    SemAST.LvalueString {s=lval_str; meta={typ = Some TYPE_stringconst}} 

  | ParserAST.LvalueArr {arr=(lval_arr, idx_expr); meta=_} -> 
    (* represents an array element and thus has the value of it's element *)
    let sem_lval_arr = check_lval lval_arr in (* check and find type of element (lval_arr) turning it into SemAST *)
    let sem_idx_expr = check_expr idx_expr in (* same *)
    (* check that idx evaluates to int *)
    if get_type sem_idx_expr <> Some TYPE_int then (raise (SemError "id of lval array not an int")) else
    (* can you index with an char ? *)
    SemAST.LvalueArr {arr=(sem_lval_arr, sem_idx_expr); meta={typ = get_type2 sem_lval_arr}} 

and check_func_call f = 
  match f with 
  | FuncCall(fcall) ->

    (* check the types, pass mode and number of actual and formal parameters are equal *)
    let actual_params = fcall.parameters in
    
    (* find the corresponding symtable entry *)
    let f_entry = lookupEntry (id_make fcall.name) LOOKUP_ALL_SCOPES true in (* all scopes ?? *)
    (* what happens if not found? 
        does id_make produce the same ids each time? *)
    let (formal_params_entries, ret_typ) = match f_entry.entry_info with
    | ENTRY_function inf -> (inf.function_paramlist, inf.function_result)
    | _ -> (raise (InternalSemError "found a non function entry when looking up a function")) in

    let get_type_and_mode e = 
      match e.entry_info with
      | ENTRY_parameter inf -> (Some inf.parameter_type, inf.parameter_mode)
      | _ -> (raise (InternalSemError "found a non param entry in function's params")) in

    let formal_params = List.map get_type_and_mode formal_params_entries in
    
    let check_params fp ap = 
      let sem_ap = check_expr ap in
      let (fp_typ, fp_mode) = fp in
      let is_lvalue = match sem_ap with
      | Lvalue _ -> true 
      | _ -> false in
      if fp_typ <> get_type sem_ap then (raise (SemError "formal and actual parameter have different type")) 
      else if (fp_mode == PASS_BY_REFERENCE && not is_lvalue) || (fp_mode == PASS_BY_VALUE && is_lvalue) then (raise (SemError "formal and actual parameter have different pass mode"))
      else sem_ap in

    let rec check_all_params f a = 
      match (f,a) with 
      | (fh::ftl, ah::atl) -> check_params fh ah :: check_all_params ftl atl (* tha xtyphsei? *)
      | ([],[]) -> []
      | _ -> (raise (SemError "different number of formal and actual parameters")) in
    
    let sem_actual_params = check_all_params formal_params actual_params in

    SemAST.FuncCall {name=fcall.name; parameters=sem_actual_params; meta={typ=Some ret_typ}}

and check_cond c = 
  match c with 
  | ParserAST.ExprCond {l; r; op=the_op; _} -> 
    let (sem_l,sem_r) = (check_expr l, check_expr r) in
    let (tl,tr) = (get_type sem_l, get_type sem_r) in
    if not ((tl == Some TYPE_int && tr == Some TYPE_int) || 
      (tl == Some TYPE_char && tr == Some TYPE_char)) then (* kai ta 2 ints kai kai ta 2 chars h epitrepw sygkrish int me char? *)
      raise (SemError "type mismatch in comparison")
    else SemAST.ExprCond{l=sem_l; r=sem_r; op=to_sem_comp the_op; meta={typ=Some TYPE_bool}} 
  | ParserAST.CompoundCond {l; r; op=the_op; _} ->
    let sem_l = check_cond l in
    let sem_r = check_cond r in
    SemAST.CompoundCond{l=sem_l; r=sem_r; op=to_sem_logical the_op; meta={typ=Some TYPE_bool}}
  | ParserAST.NegatedCond c -> SemAST.NegatedCond (check_cond c)

and check_stmt parent_ret_type single_stmt = 
  Printf.printf "stmt\n";
  match single_stmt with 
  | ParserAST.EmptyStmt -> SemAST.EmptyStmt
  | ParserAST.Assign({lvalue=l; rvalue=r_expr; meta=_}) -> (
    
    let sem_l = check_lval l in (* find the type of l and save it to meta *)
    let sem_r = check_expr r_expr in (* find the type of r and save it to meta *) 
    (* check lvalue not an array*)
    match get_type2 sem_l with
    | Some TYPE_array _ -> 
      raise (SemError "Lvalue in assignment should not be an array")
    | _ -> (
      (* check both sides are of same type *)
      if get_type2 sem_l <> get_type sem_r then 
        raise (SemError "Type mismatch on assignment")
      else 
        SemAST.Assign({lvalue=sem_l; rvalue=sem_r; meta={typ=get_type2 sem_l}}) (* fill typ with the type *)
      )
    ) 

  | ParserAST.Block(_) -> check_block single_stmt parent_ret_type
  | ParserAST.StmtFuncCall(f) -> 
    let sem_func_call = check_func_call f in
    if get_type3 sem_func_call <> Some TYPE_nothing then (raise (SemError "stmt func call must be a procedure"))
    else SemAST.StmtFuncCall(sem_func_call)
  | ParserAST.If(i) -> 
    let sem_cond = check_cond i.if_cond in
    if get_type4 sem_cond <> Some TYPE_bool then 
      raise (SemError "condition expr does not evaluate to condition") 
    else SemAST.If {
      if_cond = sem_cond; 
      ifstmt = check_stmt parent_ret_type i.ifstmt; 
      elsestmt = (match i.elsestmt with  
      | Some s -> Some (check_stmt parent_ret_type s)
      | None -> None);
      meta = {typ=None} (* if and else parts should not have the same type right? *)
    }
  | ParserAST.While(w) ->
    SemAST.While {
      while_cond = check_cond w.while_cond;
      whilestmt = check_stmt parent_ret_type w.whilestmt;
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
      if get_type sem_e <> parent_ret_type then (raise (SemError "type mismatch between return type and return value"))
      else Some sem_e in
    SemAST.Return sem_e
              
and check_block b parent_ret_type = 
  match b with 
  | ParserAST.Block(stmt_list) ->
  Printf.printf "block\n";
  SemAST.Block (check_all (check_stmt parent_ret_type) stmt_list)

let rec check_local_def x = 
  Printf.printf "local def ";
  match x with 
  | ParserAST.FuncDef(y)  -> check_func_def y
  | ParserAST.FuncDecl(y) -> check_func_decl y
  | ParserAST.VarDef _   -> check_var_def x 

and check_func_def x =
  Printf.printf "func definition\n";  
  let sem_header = check_header x.func_def_header false in
  let sem_locals = check_all check_local_def x.func_def_local in
  let ret_type = match sem_header with SemAST.Header h -> Some h.header_ret in
  let sem_block = check_block x.func_def_block ret_type in
  SemAST.FuncDef { func_def_header=sem_header; func_def_local=sem_locals; func_def_block=sem_block; meta={typ=None}}    
  

let rec check_root root = (* why rec? *)
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