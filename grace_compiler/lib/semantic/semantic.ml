open Ast
open Symbol
(* open Symbtest *)
open Identifier
open Types

exception SemError of string * Lexing.position
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
  | SemAST.Lvalue lval -> Printf.printf "lval"; get_type2 lval
  | SemAST.ExprFuncCall f -> get_type3 f 
  | SemAST.SignedExpr i -> i.meta.typ
  | SemAST.BinExpr i -> i.meta.typ

let get_loc_fun f = 
  match f with ParserAST.FuncCall i -> i.meta 

let rec get_loc_stmt s =
  match s with
  | ParserAST.EmptyStmt -> raise (InternalSemError "Trying to get loc of empty stmt")
  | ParserAST.Assign {lvalue=_; rvalue=_; meta} -> meta
  | ParserAST.Block stmt_list -> get_loc_stmt (List.hd stmt_list) (* check size *)
  | ParserAST.StmtFuncCall f -> get_loc_fun f
  | ParserAST.If {if_cond=_; ifstmt=_; elsestmt=_; meta} -> meta
  | ParserAST.While {while_cond=_; whilestmt=_; meta} -> meta
  | ParserAST.Return {ret=_; meta} -> meta

let rec get_loc_cond c =
  match c with
  | ParserAST.ExprCond i -> i.meta
  | ParserAST.CompoundCond i -> i.meta
  | ParserAST.NegatedCond i -> get_loc_cond i  

let get_loc_lval l =
  match l with
  | ParserAST.LvalueId i -> i.meta | ParserAST.LvalueString i -> i.meta | ParserAST.LvalueArr i -> i.meta  

let get_loc_expr e = 
  match e with 
  | ParserAST.Int i -> i.meta
  | ParserAST.Char c -> c.meta
  | ParserAST.Lvalue lval -> get_loc_lval lval
  | ParserAST.ExprFuncCall f -> get_loc_fun f 
  | ParserAST.SignedExpr i -> i.meta
  | ParserAST.BinExpr i -> i.meta

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

let check_return (SemAST.Block b) typ floc = 
  (* check that this block ends with a return *)
  let ends_with_ret = 
    match (List.rev b) with 
    | [] -> false
    | (SemAST.Return _)::_ -> true
    | _ -> false
  in
  match (ends_with_ret, typ) with
  | (_, None) -> raise (InternalSemError "got none type when checking func type for return") 
  (* if it does not and is TYPE_nothing, add it *)
  | (false, Some TYPE_nothing) -> SemAST.Block (b @ [SemAST.Return {ret=None; meta={typ=None}}])
  (* if it does not and is of other type, error *)
  | (false, _) -> raise (SemError ("Control reaches the end of non-void function", floc)) 
  (* else (if it does) return the following: *)
  | (true, _) -> SemAST.Block b


let check_header h is_declaration =
  match h with 
  | ParserAST.Header(x) -> 
  Printf.printf "header\n";
  let fun_entry = newFunction (id_make x.header_id) true in (

  if is_declaration then forwardFunction fun_entry;
  openScope (); Printf.printf "opening scope\n";  
  let f fpar_tuple = 
    match fpar_tuple with (mode, id, typ) -> 
      (match mode, typ with 
      | PASS_BY_VALUE, TYPE_array {ttype=_;size=_} -> raise (SemError ("Arrays must be passed by reference", x.meta))
      | _ -> newParameter (id_make id) typ mode fun_entry true)
  in
  ignore (List.map f x.header_fpar_defs);
  endFunctionHeader fun_entry x.header_ret;
  );
  SemAST.Header {
    header_id=x.header_id;
    header_fpar_defs=x.header_fpar_defs; 
    header_ret=x.header_ret;
    meta={typ=None}
  }

let check_func_decl h =
  Printf.printf "func declaration\n";
  let sem_func_decl = SemAST.FuncDecl(check_header h true) in
  (* printSymbolTable (); *)
  closeScope ();
  sem_func_decl


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
  | ParserAST.Int {i; meta=_} -> SemAST.Int {i; meta={typ= Some TYPE_int}}
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
  | ParserAST.SignedExpr {sign=s; e; meta=parser_loc} -> 
    let sem_expr = check_expr e in 
    if not (equalType (get_type sem_expr) (Some TYPE_int)) then (raise (SemError ("applying unary operation to non int", parser_loc)))
    else SemAST.SignedExpr({sign=to_sem_uop s; e=sem_expr; meta={typ=get_type sem_expr}})
  | ParserAST.BinExpr {l; r; op; meta=parser_loc} ->
    let sem_l_expr = check_expr l in
    let sem_r_expr = check_expr r in
    if not (equalType (get_type sem_l_expr) (Some TYPE_int) && equalType (get_type sem_r_expr) (Some TYPE_int)) then
      raise (SemError ("applying binary arithmetic operation to non ints", parser_loc))
    else SemAST.BinExpr {l=sem_l_expr; r=sem_r_expr; op=to_sem_arithm op; meta={typ=Some TYPE_int}}
  
and check_lval l = 
  let get_id_typ id = (
  let e = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in (* all scopes ?? *)
    (* what happens if not found? 
        does id_make produce the same ids each time? *)
    match e.entry_info with 
    | ENTRY_variable {variable_type; variable_offset= _ } -> variable_type
    | ENTRY_parameter {parameter_type; parameter_offset=_; parameter_mode=_} -> parameter_type
    | _ -> raise (InternalSemError "found non var/param while looking up type of lval\n")) in

  match l with 
  | ParserAST.LvalueId {id=lval_id; meta=parser_loc} -> 
    (* lookup type and save it to new meta *)
    SemAST.LvalueId {id=lval_id; meta={typ = (Some (get_id_typ lval_id))}}
  
  | ParserAST.LvalueString {s=lval_str; meta=_} -> 
    SemAST.LvalueString {s=lval_str; meta={typ = Some TYPE_stringconst}} 

  | ParserAST.LvalueArr {arr=(lval_arr, idx_expr); meta=parser_loc} -> 
    Printf.printf "+++++++++++++++++++++++++++++++++++++++++++++\n";
    (* represents an array element and thus has the value of it's element *)
    let sem_lval_arr = check_lval lval_arr in 
    let sem_idx_expr = check_expr idx_expr in
    (* check that idx evaluates to int *)
    if not (equalType (get_type sem_idx_expr) (Some TYPE_int)) then (raise (SemError ("id of lval array not an int", parser_loc))) else
    (* can you index with an char ? *)
    Printf.printf "%s" (pp_typ (get_type2 sem_lval_arr));
    let get_base_arr_typ lvalarr = (* from the way lvalarr is constructed in parser, the lval field of (lval, expr) is going to be the id, which will still have type of arr.Eg in x[12], x (arr) will be of type arr[int] so we need to return the base type instead (int) *)
      match lvalarr with 
      | ParserAST.LvalueId {id; _} -> 
        (match get_id_typ id with 
        | TYPE_array {ttype;size=_} -> ttype
        | _ -> raise (InternalSemError ("type of type array expected when finding type of lval array")))
      | _ -> raise (InternalSemError ("lval of type id expected when finding type of lval array")) in
    SemAST.LvalueArr {arr=(sem_lval_arr, sem_idx_expr); meta={typ = Some (get_base_arr_typ lval_arr)}} 

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
      Printf.printf "-----\n";
      let is_lvalue = match sem_ap with
        | Lvalue _ -> Printf.printf "is lval"; true 
        | _ -> Printf.printf "not lval"; false in
      let pos = get_loc_expr ap in
      let ap_typ = get_type sem_ap in
      if not (equalType ~flexible_on_autocomplete:true fp_typ ap_typ) then (raise (SemError ((Printf.sprintf "formal and actual parameter have different type (%s vs %s)" (pp_typ fp_typ) (pp_typ ap_typ)), pos))) 
      else if (fp_mode == PASS_BY_REFERENCE && not is_lvalue) (*|| (fp_mode == PASS_BY_VALUE && not is_lvalue)*) then 
        (
          Printf.printf "%s" (pp_typ ap_typ);
          raise (SemError ("passing by refernce requires an lval as actual param", pos)))
      else sem_ap in

    let rec check_all_params f a = 
      match (f,a) with 
      | (fh::ftl, ah::atl) -> check_params fh ah :: check_all_params ftl atl (* tha xtyphsei? *)
      | ([],[]) -> []
      | _ -> (raise (SemError ("different number of formal and actual parameters", fcall.meta))) in
    
    let sem_actual_params = check_all_params formal_params actual_params in

    SemAST.FuncCall {name=fcall.name; parameters=sem_actual_params; meta={typ=Some ret_typ}}

and check_cond c = 
  match c with 
  | ParserAST.ExprCond {l; r; op=the_op; meta=parser_loc} -> 
    let (sem_l,sem_r) = (check_expr l, check_expr r) in
    let (tl,tr) = (get_type sem_l, get_type sem_r) in
    if not ((tl = Some TYPE_int && tr = Some TYPE_int) || 
      (tl = Some TYPE_char && tr = Some TYPE_char)) then
      raise (SemError ((Printf.sprintf "comparing non-ints or non-chars is not allowed (%s != %s)" (pp_typ tl) (pp_typ tr)), parser_loc))
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
  | ParserAST.Assign({lvalue=l; rvalue=r_expr; meta=parser_loc}) -> (
    
    let sem_l = check_lval l in (* find the type of l and save it to meta *)
    let sem_r = check_expr r_expr in (* find the type of r and save it to meta *) 
    (* check lvalue not an array*)
    match get_type2 sem_l with
    | Some TYPE_array _ -> 
      raise (SemError ("Lvalue in assignment should not be an array", parser_loc))
    | _ -> (
      (* check both sides are of same type *)
      let tl = get_type2 sem_l in
      let tr = get_type sem_r in
      if not (equalType ~flexible_on_autocomplete:false tl tr) then 
        raise (SemError ((Printf.sprintf "type mismatch on assignment (%s != %s)" (pp_typ tl) (pp_typ tr)), parser_loc))
      else 
        SemAST.Assign({lvalue=sem_l; rvalue=sem_r; meta={typ=get_type2 sem_l}}) (* fill typ with the type *)
      )
    ) 

  | ParserAST.Block(_) -> check_block single_stmt parent_ret_type
  | ParserAST.StmtFuncCall(f) -> 
    let sem_func_call = check_func_call f in
    if not (equalType (get_type3 sem_func_call) (Some TYPE_nothing)) then (raise (SemError ("stmt func call must be a procedure", get_loc_fun f)))
    else SemAST.StmtFuncCall(sem_func_call)
  | ParserAST.If(i) -> 
    let sem_cond = check_cond i.if_cond in
    if not (equalType (get_type4 sem_cond) (Some TYPE_bool)) then 
      raise (SemError ("condition expr does not evaluate to condition", get_loc_cond i.if_cond)) 
    else SemAST.If {
      if_cond = sem_cond; 
      ifstmt = check_stmt parent_ret_type i.ifstmt; 
      elsestmt = (match i.elsestmt with  
      | Some s -> Some (check_stmt parent_ret_type s)
      | None -> None);
      meta = {typ=None} (* if and else parts should not have the same type right? *)
    }
  | ParserAST.While(w) ->
    (* printSymbolTable (); *)
    SemAST.While {
      while_cond = check_cond w.while_cond;
      whilestmt = check_stmt parent_ret_type w.whilestmt;
      meta = {typ=None}
    }
  | ParserAST.Return({ret=e; meta=parser_loc}) ->
    match e with 
    | None -> 
      if not (equalType parent_ret_type (Some TYPE_nothing)) then 
        (raise (SemError ("this function returns nothing but is not a procedure", parser_loc))) 
      else SemAST.Return {ret=None; meta={typ=None}}
    | Some ex ->
      let sem_e = check_expr ex in
      let ret_type = get_type sem_e in
      if not (equalType ret_type parent_ret_type) then (raise (SemError ((Printf.sprintf "type mismatch between return type (%s) and return value (%s)" (pp_typ ret_type) (pp_typ parent_ret_type)), parser_loc)))
      else SemAST.Return {ret=Some sem_e; meta={typ=ret_type}}
              
and check_block b parent_ret_type = 
  match b with 
  | ParserAST.Block(stmt_list) ->
  Printf.printf "block\n";
  openScope (); Printf.printf "opening scope\n";
  let sem_b = SemAST.Block (check_all (check_stmt parent_ret_type) stmt_list) in
  (* printSymbolTable ();  *)
  closeScope ();
  sem_b 

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
  let sem_block_return = check_return sem_block ret_type x.meta in
  (* printSymbolTable (); *)
  closeScope ();
  SemAST.FuncDef { func_def_header=sem_header; func_def_local=sem_locals; func_def_block=sem_block_return; meta={typ=None}}    
  
let add_buildins () = 
  let add_func id params_tuple ret_typ = 
    let fun_entry = newFunction (id_make id) true in (
    openScope (); Printf.printf "opening scope\n";
    let f fpar_tuple = 
      match fpar_tuple with (mode, id, typ) -> 
        newParameter (id_make id) typ mode fun_entry true
    in
    ignore (List.map f params_tuple);
    closeScope ();
    endFunctionHeader fun_entry ret_typ;
    );
  in

  (* fun writeInteger (n : int) : nothing; *)
  add_func "writeInteger" [(PASS_BY_VALUE, "n", TYPE_int)] TYPE_nothing;

  (* fun writeChar(c : char) : nothing; *)
  add_func "writeChar" [(PASS_BY_VALUE, "c", TYPE_char)] TYPE_nothing;

  (* fun writeString (ref s : char[]) : nothing; *)
  add_func "writeString" [(PASS_BY_REFERENCE, "s", TYPE_array{ttype=TYPE_char; size=0})] TYPE_nothing; (* pws xeirizomai to s[] ?? *)

  (* fun readInteger () : int; *)
  add_func "readInteger" [] TYPE_int;

  (* fun readChar () : char; *)
  add_func "readChar" [] TYPE_char;

  (* fun readString (n : int; ref s : char[]) : nothing; *)
  add_func "readString" [(PASS_BY_VALUE, "n", TYPE_int); (PASS_BY_REFERENCE, "s", TYPE_array{ttype=TYPE_char; size=0})] TYPE_nothing; (* pws xeirizomai to s[] ?? *)

  (* fun ascii (c : char) : int; *)
  add_func "ascii" [(PASS_BY_VALUE, "c", TYPE_char)] TYPE_int;

  (* fun chr (n : int) : char; *)
  add_func "aschrcii" [(PASS_BY_VALUE, "n", TYPE_int)] TYPE_char;

  (* fun strlen (ref s : char[]): int; *)
  add_func "strlen" [(PASS_BY_REFERENCE, "s", TYPE_array{ttype=TYPE_char; size=0})] TYPE_int; (* pws xeirizomai to s[] ?? *)

  (* fun strcmp (ref s1, s2 : char[]): int; *)
  add_func "strcmp" [(PASS_BY_REFERENCE, "s2", TYPE_array{ttype=TYPE_char; size=0}); (PASS_BY_REFERENCE, "s1", TYPE_array{ttype=TYPE_char; size=0})] TYPE_int; (* pws xeirizomai to s[] ?? *)

  (* fun strcpy (ref trg, src : char[]) : nothing; *)
  add_func "strcpy" [(PASS_BY_REFERENCE, "trg", TYPE_array{ttype=TYPE_char; size=0}); (PASS_BY_REFERENCE, "src", TYPE_array{ttype=TYPE_char; size=0})] TYPE_nothing; (* pws xeirizomai to s[] ?? *)

  (* fun strcat (ref trg, src : char[]) : nothing; *)
  add_func "strcat" [(PASS_BY_REFERENCE, "trg", TYPE_array{ttype=TYPE_char; size=0}); (PASS_BY_REFERENCE, "src", TYPE_array{ttype=TYPE_char; size=0})] TYPE_nothing (* pws xeirizomai to s[] ?? *)


let check_main (ParserAST.Header {header_id; header_fpar_defs; header_ret; meta}) =
  let good_main = 
    match (header_id, header_fpar_defs, header_ret) with
    | ("main", [], TYPE_int) 
    | ("main", [], TYPE_nothing) -> true
    | _ -> false
  in 
  if not good_main then (raise (SemError ("main() must return int or nothing and take no arguments", meta))) else ()


let check_root = function 
  | ParserAST.FuncDef x ->
    check_main x.func_def_header;
    Printf.printf "root\n"; 
    initSymbolTable 256;
    openScope (); Printf.printf "opening scope\n";
    add_buildins ();
    let sem_func_def = check_func_def x in
    (* printSymbolTable (); *)
    closeScope ();
    sem_func_def
  | _ -> raise (InternalSemError "Exprected func def as root of ast")