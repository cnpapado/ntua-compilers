open Llvm
open Types
open CGSymbol
open Identifier
open Ast

exception CodeGenError of string
exception InternalCodeGenError of string

let context = global_context ()
let the_module = create_module context "my grace prog"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 50

let int_type = i64_type context
let char_type = i8_type context
let void_type = void_type context
let cond_type = i1_type context

let rec lltype_of = function
  | TYPE_int -> int_type
  | TYPE_char -> char_type
  | TYPE_array {ttype; size} -> 
    if size < 1 then (raise (InternalCodeGenError "lltype of array with sz<1")) 
    else array_type (lltype_of ttype) size
  | TYPE_nothing -> void_type
  | _ -> raise (InternalCodeGenError "unknown lltype")





let emit_header (SemAST.Header h) is_decl =
  Printf.printf "codegen header %s\n" h.header_id;
  let params_types = Array.of_list @@ List.map (fun p -> match p with (_,_,t) -> (lltype_of t)) h.header_fpar_defs in
  let ll_types = function_type (lltype_of h.header_ret) params_types in
  let f = (if is_decl then declare_function else define_function) h.header_id ll_types the_module in
  (* save function to symbtable *)
  let (fun_entry,_) = newFunction (id_make h.header_id) f in
  openScope (); 
  (* Set names for all arguments. *)
  Array.iteri (fun i a -> (* IGNORING PASS FOR NOW *)
  let mode,name,typ = (Array.of_list h.header_fpar_defs).(i) in
    set_value_name name (params f).(i);
    (* save params to symbtable *)
    ignore @@ newVariable (id_make name) (params f).(i)
  ) (params f);
  
  endFunctionHeader fun_entry h.header_ret;  
  f

let emit_func_decl (SemAST.FuncDecl decl) = Printf.printf "codegen decl %s\n" ""; ignore @@ emit_header decl false

let emit_var_def v = 
  Printf.printf "codegen vardef%s\n" "";
  match v with 
  | SemAST.VarDef(x) -> 
    let alloca_val id = build_alloca (lltype_of x.var_def_typ) id builder in
    let add_to_symb id alloca = newVariable (id_make id) alloca in
    List.iter (fun id -> let a=alloca_val id in ignore @@ add_to_symb id a) x.var_def_id  (* fix meeeeeeeeeeeeeeeeeeeee *)

let rec emit_expr e = match e with
  | SemAST.Int {i; meta=_} -> const_int int_type i
  | SemAST.Char {c; meta=_} -> Printf.printf "%c" c; const_int char_type @@ Char.code c
  | SemAST.Lvalue(lval) -> build_load (emit_lval lval) "load" builder
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
  
and emit_lval l = match l with 
  | SemAST.LvalueId {id; meta=_} -> (* find and return from symb table *)
    let e = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
    begin
    match e.entry_info with 
    | ENTRY_variable {llval} -> llval 
    | _ -> raise (InternalCodeGenError "found non var while looking up lvalid\n")
    end
  | SemAST.LvalueString {s; meta=_} -> 
    let vl = const_stringz context s in 
    let str = define_global ".str" vl the_module in 
    set_unnamed_addr true str; 
    set_global_constant true str; 
    set_linkage Linkage.Private str; 
    let zero = const_int int_type 0 in
    build_gep str [|zero|] "strtmp" builder
  | SemAST.LvalueArr {arr=(lval_arr, idx_expr); meta=_} -> (* check idx>0 ?? *)
    let ll_lval_arr = emit_lval lval_arr in
    let ll_idx = emit_expr idx_expr in
    let zero = const_int int_type 0 in
    build_gep ll_lval_arr [| zero; ll_idx |] "arrtmp" builder

and emit_cond c = match c with
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

and emit_stmt s = match s with
  | SemAST.EmptyStmt -> ()
  | SemAST.Assign {lvalue; rvalue; meta=_} ->
    let ll_lval = emit_lval lvalue in
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
    match lookup_function f.name the_module with (* don't i need to retrieve llvm function entry from symbol table?? *)
    | Some callee -> callee
  in
  let ll_args = Array.of_list @@ List.map emit_expr f.parameters in (* for each param, if byref emit_lval, if byval, emit_expr ?? *)
  (* type cast in args ?? *)
  (* handle arr[] *)
  let ll_fname = match f.meta.typ with None -> "" | Some _ -> "call" in
  build_call callee ll_args ll_fname builder

              
and emit_block b = 
  Printf.printf "codegen block%s\n" "";
  match b with 
  | SemAST.Block(stmt_list) -> List.map emit_stmt stmt_list 


let rec emit_local_def x = 
  Printf.printf "codegen localdef%s\n" "";
  match x with 
  | SemAST.FuncDef _ -> emit_func_def x
  | SemAST.FuncDecl _ -> emit_func_decl x
  | SemAST.VarDef _  -> emit_var_def x

and emit_func_def (SemAST.FuncDef def) =
  Printf.printf "codegen func def%s\n" "";
  (* Store current block so that the builder can be repositioned there *)
  let curr_block = insertion_block builder in
  
  let header_val = emit_header def.func_def_header false in
  (* let bb = append_block context "entry" header_val in *)
  position_at_end (entry_block header_val) builder;
  
  List.iter emit_local_def def.func_def_local;
  ignore @@ emit_block def.func_def_block;
  (* match def.meta.typ with 
  | Some t ->
    let ret = begin
    match (lookupEntry (id_make "return") LOOKUP_ALL_SCOPES true).entry_info with 
    | ENTRY_variable {llval} -> llval
    | _ -> raise (InternalCodeGenError "found non var while looking up return\n")
    end
    ignore @@ build_ret ret builder;
  | None -> ignore @@ build_ret (const_int int_type 0) builder; *)
  position_at_end curr_block builder;
  ()


let emit_builtins () =  
  let declare_fun fname fparams ret_type = 
    let params_types = Array.map (fun p -> match p with (_,_,t) -> (lltype_of t)) fparams in
    let ll_types = function_type (lltype_of ret_type) params_types in
    let f = declare_function fname ll_types the_module in
    
    (* Set names for all arguments. *)
    Array.iteri (fun i a -> (* IGNORING PASS FOR NOW *)
    let name = match fparams.(i) with (_,n,_) -> n in
      set_value_name name (params f).(i);
      Hashtbl.add named_values name (params f).(i);
    ) (params f);
    
    ()
  in 

  declare_fun "writeInteger" [|(PASS_BY_VALUE, "n", TYPE_int)|] TYPE_nothing;
  declare_fun "writeChar" [|(PASS_BY_VALUE, "c", TYPE_char)|] TYPE_nothing;
  (* declare_fun "writeString" [|(PASS_BY_REFERENCE, "s", TYPE_array{ttype=TYPE_char; size=0})|] TYPE_nothing; *) (* pws xeirizomai to s[] ?? *)
  declare_fun "readInteger" [||] TYPE_int;
  declare_fun "readChar" [||] TYPE_char;
  (* declare_fun "readString" [|(PASS_BY_VALUE, "n", TYPE_int); (PASS_BY_REFERENCE, "s", TYPE_array{ttype=TYPE_char; size=0})|] TYPE_nothing; *) (* pws xeirizomai to s[] ?? *)
  declare_fun "ascii" [|(PASS_BY_VALUE, "c", TYPE_char)|] TYPE_int;
  declare_fun "aschrcii" [|(PASS_BY_VALUE, "n", TYPE_int)|] TYPE_char
  (* declare_fun "strlen" [|(PASS_BY_REFERENCE, "s", TYPE_array{ttype=TYPE_char; size=0})|] TYPE_int; (* pws xeirizomai to s[] ?? *)
  declare_fun "strcmp" [|(PASS_BY_REFERENCE, "s2", TYPE_array{ttype=TYPE_char; size=0}); (PASS_BY_REFERENCE, "s1", TYPE_array{ttype=TYPE_char; size=0})|] TYPE_int; (* pws xeirizomai to s[] ?? *)
  declare_fun "strcpy" [|(PASS_BY_REFERENCE, "trg", TYPE_array{ttype=TYPE_char; size=0}); (PASS_BY_REFERENCE, "src", TYPE_array{ttype=TYPE_char; size=0})|] TYPE_nothing; (* pws xeirizomai to s[] ?? *)
  declare_fun "strcat" [|(PASS_BY_REFERENCE, "trg", TYPE_array{ttype=TYPE_char; size=0}); (PASS_BY_REFERENCE, "src", TYPE_array{ttype=TYPE_char; size=0})|] TYPE_nothing pws xeirizomai to s[] ?? *)

(* SCOPEEEEEEEEEEEEEEEEEEEEEEEEEEEES *)


let emit_root r = 
  emit_builtins ();
  let head, locals, block = match r with SemAST.FuncDef def -> def.func_def_header, def.func_def_local, def.func_def_block in
  (* Hashtbl.clear named_values; *)
  let main_type = function_type void_type [||] in
  let main = declare_function "global_sco" main_type the_module in
  let bb = append_block context "entry" main in
  position_at_end bb builder; 
  ignore @@ List.map emit_local_def locals;
  (* ignore @@ emit_block block; *)
  ignore @@ build_ret_void builder; 
  ()
