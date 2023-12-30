open Llvm

exception CodeGenError of string
exception InternalCodeGenError of string

let context = global_context ()
let the_module = create_module context "my grace prog"
let builder = builder context
(* let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10 *)
(* let double_type = double_type context *)
let int_type = i64_type context
let char_type = i8_type context
let void_type = void_type context
let cond_type = i1_type context

(* let rec lltype_of grace_t = function
  | TYPE_int -> int_type
  | TYPE_char -> char_type
  | TYPE_array {ttype; size} -> 
    if size < 1 then (raise (InternalCodeGenError "lltype of array with sz<1")) 
    else array_type (lltype_of ttype) size
  | _ -> raise (InternalCodeGenError "unknown lltype") *)





(* let emit_header 
  
let emit_func_decl 
*)

(* let emit_var_def v =
  (* possibly add to symbol table? *)
  match v with 
  | ParserAST.VarDef(x) ->  *)

let rec emit_expr e = function
  | SemAST.Int {i; meta=_} -> const_int int_type i
  | SemAST.Char {c; meta=_} -> const_int char_type c
  | SemAST.Lvalue(lval) -> emit_lval lval
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
  
and emit_lval l = function 
  | SemAST.LvalueId {id=lval_id; meta=_} -> (* find and return from symb table *)
  | SemAST.LvalueString of {s; meta=_} -> 
    let vl = const_stringz context s in 
    let str = define_global ".str" vl module in 
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

and emit_cond c = function
  | SemAST.ExprCond {l; r; op; meta=_} -> 
    let ll_l = emit_expr l in
    let ll_r = emit_expr r in
    match op with 
    | Lt  -> build_icmp Icmp.Slt ll_l ll_r "lttmp" builder
    | Gt  -> build_icmp Icmp.Sgt ll_l ll_r "gttmp" builder
    | Le  -> build_icmp Icmp.Sle ll_l ll_r "letmp" builder
    | Ge  -> build_icmp Icmp.Sge ll_l ll_r "getmp" builder
    | Eq  -> build_icmp Icmp.Eq ll_l ll_r "eqtmp" builder
    | Neq -> build_icmp Icmp.Ne ll_l ll_r "netmp" builder
  | SemAST.CompoundCond {l; r; op; _} ->
    let ll_l = emit_expr l in
    let ll_r = emit_expr r in
    match op with 
    | And -> build_and ll_l ll_r "andtmp" builder
    | Or -> build_or ll_l ll_r "ortmp" builder
  | SemAST.NegatedCond c ->  
    let ll_c = emit_cond c in build_not ll_c "negtmp" builders 

and emit_stmt s = function
  | SemAST.EmptyStmt -> ()
  | SemAST.Assign {lvalue; rvalue; meta=_} -> 
    let ll_lval = emit_lval lvalue in
    let ll_rval = emit_expr rvalue in
    ignore @@ build_store ll_rval ll_lval builder (* type cast ?? *)
  | SemAST.Block b -> emit_block b
  | SemAST.StmtFuncCall s -> emit_func_call s
  | SemAST.If i -> 
    let cond = build_trunc (emit_expr i.if_cond) cond_type "trunc" builder in
    let cur_bb = insertion_block builder in
    let parent = block_parent cur_bb in
    let cont_bb = append_block context "cont" parent in
    (* Builds a basic block that containts stmt and jump to the continuation block *)
    let build_block name stmt =
      let bb = append_block context name parent in
      position_at_end bb builder;
      emit_stmt stmt;
      ignore @@ build_br cont_bb builder;
      bb
    in
    let then_bb = build_block "then" i.ifstmt in
    (* Optionally build else block if there is any, otherwise use the cont block *)
    let else_bb = Option.(value ~default:cont_bb @@ map (build_block "else") else_stmt_opt) in
    (* Use insertion block here because we don't know if this is the then block or the else block *)
    move_block_after (insertion_block builder) cont_bb;
    position_at_end cur_bb builder;
    (* Having set up the other blocks, finally build the branch instruction *)
    ignore @@ build_cond_br cond then_bb else_bb builder;
    position_at_end cont_bb builder
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
    let cond = build_trunc (emit_expr w.while_cond) cond_type "trunc" builder in
    ignore @@ build_cond_br cond while_bb cont_bb builder;
    position_at_end while_bb builder;
    emit_stmt w.whilestmt;
    (* Unconditional branch to the condition block *)
    ignore @@ build_br cond_bb builder;
    position_at_end cont_bb builder
  | SemAST.Return {ret; meta=_} -> 
  

and emit_func_call f = 
  (* retrieve llvm function entry from symbol table *)
  (* for each param, if byref emit_lval, if byval, emit_expr ?? *)

  (* type cast in args ?? *)
  (* handle arr[] *)
  let args = List.map emit_expr (params ll_fun) in 
  let fname = match ftype with None -> "" Some _ -> "call" in
  build_call ll_fun args fname builder

              
and emit_block b = 
  match b with 
  | ParserAST.Block(stmt_list) -> List.iter emit_stmt stmt_list

(*
let rec emit_local_def 

and emit_func_def  *)


let emit_root r = 
  (* dump_module the_module; *)
  let main_type = function_type void_type [||] in
  let main = declare_function "main" main_type the_module in
  let bb = append_block context "entry" main in
  position_at_end bb builder; 
  let lhs_val = const_float double_type 42.0 in
  let rhs_val = const_float double_type 17.0 in
  let _ = build_add lhs_val rhs_val "addtmp" builder in 
  let _ = build_ret double_type builder in
  (* dump_module the_module *) ()