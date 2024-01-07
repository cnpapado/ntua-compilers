open Llvm
open Types
open CGSymbol
open CGSymbtest
open Identifier
open Ast

exception CodeGenError of string
exception InternalCodeGenError of string
exception Autocomplete of Types.typ * llvalue


let context = global_context ()
let the_module = create_module context "my grace prog"
let builder = builder context

let int_type = i64_type context
let char_type = i8_type context
let void_type = void_type context
let cond_type = i1_type context

let rec lltype_of = function
  | TYPE_int -> int_type
  | TYPE_char -> char_type
  | TYPE_array {ttype; size} -> 
    if size < 0 then (raise (InternalCodeGenError "lltype of array with sz<1")) 
    (* else if size = 0 then pointer_type (lltype_of ttype) *)
    else array_type (lltype_of ttype) size
  | TYPE_nothing -> void_type
  | _ -> raise (InternalCodeGenError "unknown lltype")

let emit_header (SemAST.Header h) is_decl =
  Printf.printf "codegen header %s\n" h.header_id;
  let params_types = Array.of_list @@ List.map (
    fun p -> match p with 
      | (PASS_BY_VALUE,_,t) -> (lltype_of t)
      (* treat incomplete arrs as pointers to the first element (base type) *)
      | (PASS_BY_REFERENCE,_,TYPE_array {ttype=base_type; size=0}) -> pointer_type ((* pointer_type *) (lltype_of base_type))
      (* treat params which pass by reference as pointers *)
      | (PASS_BY_REFERENCE,_,t) -> pointer_type (lltype_of t)
    ) h.header_fpar_defs in
  let ll_types = function_type (lltype_of h.header_ret) params_types in
  let f = (if is_decl then declare_function else define_function) h.header_id ll_types the_module in
  (* save function to symbtable *)
  let (fun_entry,_) = newFunction (id_make h.header_id) f in
  openScope (); 
  (* Set names for all arguments. *)
  Array.iteri (fun i a -> 
  let mode,name,typ = (Array.of_list h.header_fpar_defs).(i) in
    set_value_name name (params f).(i);
    (* save params to symbtable *)
    Printf.printf "adding param %s\n" name;
    Printf.printf "params total num=%d\n" @@ Array.length (params f);
    
    if not is_decl then
    begin
    (* save current block to return later *)
    let curr_block = insertion_block builder in

    let _ = position_at_end (entry_block f) builder in
    let alloca_val = match mode with  
      | PASS_BY_VALUE ->
        let alloca_val = build_alloca (lltype_of typ) name builder in
        let _ = build_store (params f).(i) alloca_val builder in
        alloca_val
      | PASS_BY_REFERENCE ->
        (params f).(i)
    in
    
    ignore @@ newParameter (id_make name) typ mode (Some alloca_val) fun_entry;
    position_at_end curr_block builder;
    
    end
    
  ) (params f);
  
  endFunctionHeader fun_entry h.header_ret;  
  f

let emit_func_decl (SemAST.FuncDecl decl) = 
  Printf.printf "codegen decl %s\n" ""; 
  ignore @@ emit_header decl false;
  printSymbolTable ();
  closeScope ()

let emit_var_def v = 
  Printf.printf "codegen vardef%s\n" "";
  match v with 
  | SemAST.VarDef(x) -> 
    let alloca_val id = build_alloca (lltype_of x.var_def_typ) id builder in
    let add_to_symb id alloca = Printf.printf "adding vardef %s\n" id; newVariable (id_make id) alloca in
    List.iter (fun id -> let a=alloca_val id in ignore @@ add_to_symb id a) x.var_def_id 

let rec emit_expr e = Printf.printf "codegen expr%s\n" ""; match e with
  | SemAST.Int {i; meta=_} -> const_int int_type i
  | SemAST.Char {c; meta=_} -> Printf.printf "%c" c; const_int char_type @@ Char.code c
  | SemAST.Lvalue(lval) -> (* read *)
    begin
    match lval with 
    | SemAST.LvalueId _ -> emit_lval lval true (* if it's a var id load, but if it's a param, return from symbt *)
    | SemAST.LvalueString _ 
    | SemAST.LvalueArr _ -> emit_lval lval true
    end
  | SemAST.ExprFuncCall(func_call) -> emit_func_call func_call
  | SemAST.SignedExpr {sign; e; meta=_} -> let ll_e = emit_expr e in build_neg ll_e "negtmp" builder
  | SemAST.BinExpr {l; r; op; meta=_} ->
    let lval = emit_expr l in
    let rval = emit_expr r in
    match op with 
    | Times -> build_mul lval rval "multmp" builder
    | Div   -> build_sdiv lval rval "divtmp" builder
    | Mod   -> build_srem lval rval "modtmp" builder
    | Plus  -> build_add lval rval "addtmp" builder
    | Minus -> build_sub lval rval "subtmp" builder
  
and emit_lval l make_load = Printf.printf "codegen lval%s\n" ""; match l with 
  | SemAST.LvalueId {id; meta=_} -> 
    (* find and return from symb table *)
    let e = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
    begin
    match e.entry_info with 
    (* if make_load then this is a reference (as opposed to a write) on this var and you need to load it first *)
    | ENTRY_variable {llval} -> if make_load then build_load llval "load" builder else llval  (* MAYBE CHANGE ME *)
    | ENTRY_parameter {parameter_type; parameter_mode=_; llp} -> 
      begin
        match llp with 
        | Some llp -> 
          begin 
            match parameter_type with
            (*  *)
            | TYPE_array {ttype=base_type; size=0} -> raise (Autocomplete (base_type,llp));
              (* build_load llp "load" builder *)
            | _ -> if make_load then build_load llp "load" builder else llp 
          end
        | None -> raise (InternalCodeGenError "found null llvalue while looking up param")
      end
    | _ -> raise (InternalCodeGenError "found non var while looking up lval\n")
    end
  | SemAST.LvalueString {s; meta=_} -> 
    let vl = const_stringz context s in 
    let str = define_global ".str" vl the_module in 
    set_unnamed_addr true str; 
    set_global_constant true str; 
    set_linkage Linkage.Private str; 
    let zero = const_int int_type 0 in
    build_gep str [|zero|] "strtmp" builder
  | SemAST.LvalueArr {arr=(lval_arr, idx_expr); meta={typ=Some (TYPE_array _)}} ->
    raise (InternalCodeGenError "generating for a[]")
  | SemAST.LvalueArr {arr=(lval_arr, idx_expr); meta=_} -> (* check idx>0 ?? *)
    let ll_idx = emit_expr idx_expr in
    let zero = const_int int_type 0 in
    try 
      let ll_lval_arr = emit_lval lval_arr false in
      let gep = build_gep ll_lval_arr [| zero; ll_idx |] "arrtmp" builder in
      if make_load then build_load gep "load_arr_elem" builder
      else gep
    with Autocomplete (base_type, ll_lval_arr) -> 
      (* this is an autocomplete arr *)
      (* let ll_lval_arr = emit_lval lval_arr false in *)
      (* let cast = build_bitcast ll_lval_arr (lltype_of base_type) "bitcast_byref_arg" builder in *)

      (* let gep = build_gep ll_lval_arr [| zero |] "arrtmp" builder in *)
      (* let load_arr = build_load gep "load_arr_auto" builder in *)
      let gep = build_gep ll_lval_arr [| ll_idx |] "arrtmp" builder in
      if make_load then build_load gep "load_arr_elem" builder 
      else gep

and emit_cond c = Printf.printf "codegen cond%s\n" ""; match c with  
  | SemAST.ExprCond {l; r; op; meta=_} -> 
    let ll_l = emit_expr l in
    let ll_r = emit_expr r in
    begin
    match op with 
    | Lt  -> build_icmp Icmp.Slt ll_l ll_r "lttmp" builder
    | Gt  -> build_icmp Icmp.Sgt ll_l ll_r "gttmp" builder
    | Le  -> build_icmp Icmp.Sle ll_l ll_r "letmp" builder
    | Ge  -> build_icmp Icmp.Sge ll_l ll_r "getmp" builder
    | Eq  -> build_icmp Icmp.Eq ll_l ll_r "eqtmp" builder
    | Neq -> build_icmp Icmp.Ne ll_l ll_r "netmp" builder
    end
  | SemAST.CompoundCond {l; r; op; _} ->
    let ll_l = emit_cond l in
    let ll_r = emit_cond r in
    begin
    match op with 
    | And -> build_and ll_l ll_r "andtmp" builder
    | Or -> build_or ll_l ll_r "ortmp" builder
    end
  | SemAST.NegatedCond c ->  
    let ll_c = emit_cond c in build_not ll_c "negtmp" builder

and emit_stmt s = Printf.printf "codegen stmt%s\n" ""; match s with
  | SemAST.EmptyStmt -> ()
  | SemAST.Assign {lvalue; rvalue; meta=_} ->
    let ll_lval = emit_lval lvalue false in
    let ll_rval = emit_expr rvalue in
    ignore @@ build_store ll_rval ll_lval builder (* type cast ?? *)
  | SemAST.Block _ -> ignore @@ emit_block s
  | SemAST.StmtFuncCall s -> ignore @@ emit_func_call s
  | SemAST.If i -> 
    let cond = build_trunc (emit_cond i.if_cond) cond_type "trunc" builder in
    let cur_bb = insertion_block builder in
    let parent = block_parent cur_bb in
    let cont_bb = append_block context "cont" parent in
    (* Builds a basic block that containts stmt and jump to the continuation block *)
    let build_block name stmt =
      let bb = append_block context name parent in
      position_at_end bb builder;
      ignore @@ emit_stmt stmt;
      ignore @@ build_br cont_bb builder;
      bb
    in
    let then_bb = build_block "then" i.ifstmt in
    (* Optionally build else block if there is any, otherwise use the cont block *)
    let else_bb = Option.(value ~default:cont_bb @@ map (build_block "else") i.elsestmt) in
    (* Use insertion block here because we don't know if this is the then block or the else block *)
    move_block_after (insertion_block builder) cont_bb;
    position_at_end cur_bb builder;
    (* Having set up the other blocks, finally build the branch instruction *)
    ignore @@ build_cond_br cond then_bb else_bb builder;
    position_at_end cont_bb builder;
    () 
  | SemAST.While w ->
    let parent = block_parent @@ insertion_block builder in
    (* The basic block evaluating the condition *)
    let cond_bb = append_block context "cond" parent in
    (* The basic block containing the body of the while loop *)
    let while_bb = append_block context "while" parent in
    (* The basic block the comes after *)
    let cont_bb = append_block context "cont" parent in
    ignore @@ build_br cond_bb builder;
    position_at_end cond_bb builder;
    (* Trunc is needed because llvm requires i1 type for conditions in branch instructions *)
    let cond = build_trunc (emit_cond w.while_cond) cond_type "trunc" builder in
    ignore @@ build_cond_br cond while_bb cont_bb builder;
    position_at_end while_bb builder;
    ignore @@ emit_stmt w.whilestmt;
    (* Unconditional branch to the condition block *)
    ignore @@ build_br cond_bb builder;
    position_at_end cont_bb builder; 
    ()
  | SemAST.Return {ret; meta} -> 
    match ret with 
    | Some r -> ignore @@ build_ret (emit_expr r) builder
    | None -> ignore @@ build_ret_void builder

  

and emit_func_call (SemAST.FuncCall f) = 
  Printf.printf "codegen func call%s\n" "";
  (* Look up the name in the module table. *)
  let callee =
    match lookup_function f.name the_module with
    | Some callee -> callee
    | None -> (raise (InternalCodeGenError ("failed looking up function " ^ f.name ^ " in module")))
  in
  List.iter (fun p -> Printf.printf "%s, " @@ Pretty_print.str_of_expr p) f.parameters;
  let formal_parameters = 
    let f_entry = lookupEntry (id_make f.name) LOOKUP_ALL_SCOPES true in 
    let formal_params_entries = match f_entry.entry_info with
    | ENTRY_function inf -> inf.function_paramlist
    | _ -> (raise (InternalCodeGenError "found a non function entry when looking up a function")) in
    List.map (fun e -> match e.entry_info with ENTRY_parameter p -> p.parameter_mode, p.parameter_type ) formal_params_entries
  in
  let ll_args = Array.of_list @@ List.map2 (
    fun arg formal_param -> 
      match formal_param with 
      | PASS_BY_VALUE,_ -> emit_expr arg
      (* if its an autocomplete array pass the addr of that *)
      | PASS_BY_REFERENCE, TYPE_array {ttype=base_type; size=0} ->
        begin
        match arg with 
        | SemAST.Lvalue lval -> (* build_gep (emit_lval lval false) [| (const_int int_type 0); |] "gep" builder *)
          let cast = build_bitcast (emit_lval lval false) (pointer_type ((*pointer_type*) int_type)) "bitcast_byref_arg" builder in
          cast
          (* begin match lval with 
          | SemAST.LvalueArr {arr=(_, _); meta=_} -> emit_lval lval false
          | _ -> raise (InternalCodeGenError "1")
          end *)
        | _ -> raise (InternalCodeGenError "expecting Lvalue for arg")
        end
      (* if its another byref arg, pass the addr of this lval *)
      | PASS_BY_REFERENCE,_ -> 
        (match arg with SemAST.Lvalue lval -> emit_lval lval false) 
    ) f.parameters formal_parameters in
  (* type cast in args ?? *)
  (* handle arr[] *)
  let ll_fname = match f.meta.typ with None -> "" | Some _ -> "call" in
  build_call callee ll_args ll_fname builder

              
and emit_block b = 
  Printf.printf "codegen block%s\n" "";
  openScope ();
  let ll_stmt_list = match b with 
    | SemAST.Block(stmt_list) -> List.map emit_stmt stmt_list in
  printSymbolTable (); 
  closeScope ();
  ll_stmt_list

let rec emit_local_def x = 
  Printf.printf "codegen localdef%s\n" "";
  match x with 
  | SemAST.FuncDef _ -> emit_func_def x
  | SemAST.FuncDecl _ -> emit_func_decl x
  | SemAST.VarDef _  -> emit_var_def x

and emit_func_def (SemAST.FuncDef def) =
  Printf.printf "codegen func def%s\n" "";
  (* save current block to return later *)
  let curr_block = insertion_block builder in
  
  let header_val = emit_header def.func_def_header false in
  position_at_end (entry_block header_val) builder;
  
  List.iter emit_local_def def.func_def_local;
  ignore @@ emit_block def.func_def_block;
  printSymbolTable ();
  closeScope ();
  position_at_end curr_block builder;
  ()


let emit_builtins () =  
  let declare_fun fname fparams ret_type = 
    let params_types = Array.map (fun p -> match p with (_,_,t) -> (lltype_of t)) fparams in
    let ll_types = function_type (lltype_of ret_type) params_types in
    let f = declare_function fname ll_types the_module in
    (* save function to symbtable *)
    let (fun_entry,_) = newFunction (id_make fname) f in
    openScope (); 
    (* Set names for all arguments. *)
    Array.iteri (fun i a -> (* IGNORING PASS FOR NOW *)
    let mode,name,typ = fparams.(i) in
      set_value_name name (params f).(i);
      ignore @@ newParameter (id_make fname) typ mode None fun_entry;
    ) (params f);

    closeScope ();
    endFunctionHeader fun_entry ret_type; 
    
    ()
  in 

  declare_fun "writeInteger" [|(PASS_BY_VALUE, "n", TYPE_int)|] TYPE_nothing;
  declare_fun "writeChar" [|(PASS_BY_VALUE, "c", TYPE_char)|] TYPE_nothing;
  (* declare_fun "writeString" [|(PASS_BY_REFERENCE, "s", TYPE_array{ttype=TYPE_char; size=0})|] TYPE_nothing; pws xeirizomai to s[] ?? *)
  declare_fun "readInteger" [||] TYPE_int;
  declare_fun "readChar" [||] TYPE_char;
  (* declare_fun "readString" [|(PASS_BY_VALUE, "n", TYPE_int); (PASS_BY_REFERENCE, "s", TYPE_array{ttype=TYPE_char; size=0})|] TYPE_nothing; pws xeirizomai to s[] ?? *)
  declare_fun "ascii" [|(PASS_BY_VALUE, "c", TYPE_char)|] TYPE_int;
  declare_fun "aschrcii" [|(PASS_BY_VALUE, "n", TYPE_int)|] TYPE_char
  (* declare_fun "strlen" [|(PASS_BY_REFERENCE, "s", TYPE_array{ttype=TYPE_char; size=0})|] TYPE_int; (* pws xeirizomai to s[] ?? *)
  declare_fun "strcmp" [|(PASS_BY_REFERENCE, "s2", TYPE_array{ttype=TYPE_char; size=0}); (PASS_BY_REFERENCE, "s1", TYPE_array{ttype=TYPE_char; size=0})|] TYPE_int; (* pws xeirizomai to s[] ?? *)
  declare_fun "strcpy" [|(PASS_BY_REFERENCE, "trg", TYPE_array{ttype=TYPE_char; size=0}); (PASS_BY_REFERENCE, "src", TYPE_array{ttype=TYPE_char; size=0})|] TYPE_nothing; (* pws xeirizomai to s[] ?? *)
  declare_fun "strcat" [|(PASS_BY_REFERENCE, "trg", TYPE_array{ttype=TYPE_char; size=0}); (PASS_BY_REFERENCE, "src", TYPE_array{ttype=TYPE_char; size=0})|] TYPE_nothing pws xeirizomai to s[] ?? *)



let emit_root r = 
  initSymbolTable 256;
  emit_builtins ();
  openScope ();
  let head, locals, block = match r with SemAST.FuncDef def -> def.func_def_header, def.func_def_local, def.func_def_block in
  
  (* define main *)
  let main_type = function_type void_type [||] in
  let main = define_function "main" main_type the_module in
  position_at_end (entry_block main) builder; 
  ignore @@ List.map emit_local_def locals;
  (* emit body which contains call to top-level function of the program *)
  ignore @@ emit_block block;
  (* return 0 *)
  ignore @@ build_ret (const_int int_type 0) builder; 
  
  printSymbolTable ();
  closeScope ();
  ()



let emit_root2 r = 
  emit_builtins ();
  let zero = const_int int_type 0 in
  (* 
   * define main 
   *)
  let main_type = function_type int_type [||] in
  let main = define_function "main" main_type the_module in
  position_at_end (entry_block main) builder; 

  (*  
   * define another function, f
   *)

  (* save current block to return later *)
  let curr_block = insertion_block builder in

  let f_type = function_type int_type [| pointer_type (pointer_type int_type) |] in
  let f = define_function "f" f_type the_module in
  position_at_end (entry_block f) builder; 
  
  (* load a[0] *)

  let ptr1 = build_gep (params f).(0) [| zero; |] "gep" builder in
  let arr = build_load ptr1 "load" builder in
  let my_int = build_gep arr [| zero; |] "gep" builder in
  (* let ret = build_load ptr "load" builder in *)
  
  (* let rhs_val = const_int int_type 1 in *)
  (* add 1 to the derefed ptr val *)
  (* let ret = build_add lhs_val rhs_val "addtmp" builder in *)
  (* save it back to ptr *)
  (* let _ = build_store ret (params f).(0) builder in *)
  
  
  (* return it *)
  ignore @@ build_ret my_int builder;

  (* return to main block *)
  position_at_end curr_block builder;

  (*
   * continue with main's definition
   *)
  let alloca_val = build_alloca (array_type int_type 2) "my_arr" builder in
  (* let num = build_store (const_int int_type 42) alloca_val builder in *)

  (*
     
    I AM RETURNIG a[0] through f() but I have not been allocating it!
  
  *)


  let build_funcall = 
    (* Look up the name in the module table. *)
    let callee =
      match lookup_function "f" the_module with
      | Some callee -> callee
    in
    let args = [| alloca_val |] (*Array.map codegen_expr args*) in
    build_call callee args "" builder in


  ignore @@ build_ret build_funcall builder