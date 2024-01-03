open Ast

let free_vars_hashtbl = Hashtbl.create 256

(* Return a list of the free variables of a function definition *)
let free_vars (SemAST.FuncDef def) = 
  let params = 
    List.map (fun (_,id,typ) -> (id,typ)) (match def.func_def_header with Header h -> h.header_fpar_defs) in
  let locals =
    List.concat @@ List.map
    (fun ldef -> match ldef with
      | SemAST.FuncDef _ 
      | SemAST.FuncDecl _ -> []
      | SemAST.VarDef v -> List.map (fun id -> (id, v.var_def_typ)) v.var_def_id
    )
    def.func_def_local
  in 
  let referenced = (* traverse and append all ids to list *) 
    (* return l appended with all ids found in stmt *)
    let rec collect_stmt l stmt = 
      match stmt with 
      | SemAST.EmptyStmt -> l
      | SemAST.Assign {lvalue; rvalue; meta=_} -> List.concat [l; (collect_lval l lvalue); (collect_expr [] rvalue)]
      | SemAST.Block stmt_list -> List.fold_left collect_stmt l stmt_list (* collect_stmt (collect_stmt (collect_stmt l b0) b1) b2 *)
      | SemAST.StmtFuncCall f -> (match f with SemAST.FuncCall ff -> (ff.name, ff.meta.typ)) :: l
      | SemAST.If i -> List.concat [(collect_cond l i.if_cond); (collect_stmt [] i.ifstmt); (match i.elsestmt with Some e -> collect_stmt [] e | None -> [])]
      | SemAST.While w -> List.concat [(collect_cond l w.while_cond); (collect_stmt l w.whilestmt)]
      | SemAST.Return {ret; meta=_} -> match ret with Some sr -> collect_expr l sr | None -> l
    and collect_lval l lval =
      match lval with 
      | LvalueId {id; meta} -> (id, meta.typ) :: l 
      | LvalueString _ -> l
      | LvalueArr {arr=(lval_arr, idx_expr); meta=_} -> (collect_lval l lval_arr) @ (collect_expr [] idx_expr)
    and collect_expr l e = 
      match e with
      | SemAST.Int i -> l
      | SemAST.Char c -> l
      | SemAST.Lvalue lval -> collect_lval l lval
      | SemAST.ExprFuncCall f -> (match f with SemAST.FuncCall ff -> (ff.name, ff.meta.typ)) :: l
      | SemAST.SignedExpr se -> collect_expr l se.e
      | SemAST.BinExpr b -> (collect_expr l b.l) @ (collect_expr [] b.r)
    and collect_cond l c = match c with 
      | SemAST.ExprCond c -> (collect_expr l c.l) @ (collect_expr [] c.r)
      | SemAST.CompoundCond c -> (collect_cond l c.l) @ (collect_cond [] c.r)
      | SemAST.NegatedCond c -> collect_cond l c
    in 
    collect_stmt [] def.func_def_block
  in
    let diff l1 l2 = List.filter (fun (id,typ) -> not (List.mem (id,typ) l2) && not (Rename.SS.mem id !Rename.function_names)) l1 in
    let referenced = (* decapsulate options *)
      List.map (fun (id,t) -> match t with Some tt -> (id,tt)) referenced in
    let free = diff referenced (params @ locals) in
    free

let add_actual_params f params =
  let new_fdefs = List.map (fun (free_id,free_typ) -> (Symbol.PASS_BY_REFERENCE, free_id, free_typ)) params in
  let new_head head = match head with SemAST.Header h -> 
    SemAST.Header {h with header_fpar_defs= h.header_fpar_defs @ new_fdefs} in
  match f with 
  | SemAST.FuncDef def -> SemAST.FuncDef {def with func_def_header=(new_head def.func_def_header)}
  | SemAST.FuncDecl decl -> SemAST.FuncDecl (new_head decl) 
  
let add_typical_params (SemAST.FuncCall call) params =
  let new_args = List.map 
    (fun (free_id,free_typ) -> (SemAST.Lvalue (SemAST.LvalueId {id=free_id; meta={typ=Some free_typ}}))) 
    params in
  SemAST.FuncCall {call with parameters=call.parameters @ new_args}

(* traverse ast and replace free vars with params on all defs, decls and calls *)
let rec replace_free fun_def =  
  Printf.printf "%s\n" "replace_free"; 
  (* find my own free vars *)  
  let free = free_vars fun_def in
  (* append them to my own params *) 
  let fun_def_with_own_appended = add_actual_params fun_def free in 
  let (my_id, my_locals, my_block) = 
    match fun_def_with_own_appended with 
      SemAST.FuncDef {func_def_header; func_def_local; func_def_block; meta=_} ->
        (match func_def_header with SemAST.Header h -> h.header_id, func_def_local, func_def_block) in
  (* save them also to hashtbl *)
  Hashtbl.add free_vars_hashtbl my_id free;
  
  (* 
  on my local defs: 
    - handle defs first (do recursion) Hashtbl.find free_vars_hashtbl (match decl with SemAST.Header h -> h.header_id
    - for decls, lookup hashtbl and add params
  *)
  let my_new_locals = 
    let replace_defs = List.map (fun d -> 
    match d with 
    | SemAST.FuncDef _ -> replace_free d
    | _ -> d
    ) in replace_defs my_locals in
    
    
    (* | SemAST.FuncDecl decl ->
      let free = Hashtbl.find free_vars_hashtbl (match decl with SemAST.Header h -> h.header_id) in
      add_actual_params d free
    | SemAST.VarDef _ -> d) my_locals in *)
  
  (*
  on my body:
    - find all call sites and add params (from hashtbl) 
  *)
  let my_new_body = 
    let rec append_call_sites stmt = 
      match stmt with 
      | SemAST.EmptyStmt -> SemAST.EmptyStmt 
      | SemAST.Assign {lvalue; rvalue; meta} -> 
        SemAST.Assign {
          lvalue=append_call_sites_lval lvalue;
          rvalue=append_call_sites_expr rvalue;
          meta
        } 
      | SemAST.Block stmt_list -> SemAST.Block (List.map append_call_sites stmt_list)
      | SemAST.StmtFuncCall f -> 
        let free = Hashtbl.find free_vars_hashtbl (match f with SemAST.FuncCall ff -> ff.name) in
        SemAST.StmtFuncCall (add_typical_params f free)
      | SemAST.If {if_cond; ifstmt; elsestmt; meta} ->
        SemAST.If {
          if_cond=append_call_sites_cond if_cond;
          ifstmt=append_call_sites ifstmt; 
          elsestmt=(match elsestmt with Some e -> Some (append_call_sites e) | None -> None);
          meta
        }
      | SemAST.While {while_cond; whilestmt; meta} ->
        SemAST.While {
          while_cond=append_call_sites_cond while_cond;
          whilestmt=append_call_sites whilestmt;
          meta
        }
      | SemAST.Return {ret; meta} -> 
        SemAST.Return {ret=(match ret with Some r -> Some (append_call_sites_expr r) | None -> None); meta} 
    and append_call_sites_lval lval = match lval with 
      | SemAST.LvalueArr {arr=(lval_arr, expr_idx); meta} -> SemAST.LvalueArr {arr=(append_call_sites_lval lval_arr, append_call_sites_expr expr_idx); meta}
      | _ -> lval
    and append_call_sites_cond c = match c with 
      | SemAST.ExprCond c -> SemAST.ExprCond {l=(append_call_sites_expr c.l); r=(append_call_sites_expr c.r); op=c.op; meta=c.meta;} 
      | SemAST.CompoundCond c -> SemAST.CompoundCond {l=(append_call_sites_cond c.l); r=(append_call_sites_cond c.r); op=c.op; meta=c.meta;} 
      | SemAST.NegatedCond c -> SemAST.NegatedCond (append_call_sites_cond c)
    and append_call_sites_expr e = match e with
      | SemAST.Int i -> e
      | SemAST.Char c -> e
      | SemAST.Lvalue lval -> SemAST.Lvalue (append_call_sites_lval lval)
      | SemAST.ExprFuncCall f -> 
        let free = Hashtbl.find free_vars_hashtbl (match f with SemAST.FuncCall ff -> ff.name) in
        SemAST.ExprFuncCall (add_typical_params f free)
      | SemAST.SignedExpr se -> SemAST.SignedExpr {sign=se.sign; e=append_call_sites_expr se.e; meta=se.meta}
      | SemAST.BinExpr b -> SemAST.BinExpr {l=append_call_sites_expr b.l; r=append_call_sites_expr b.r; op=b.op; meta=b.meta}

    in 
    append_call_sites my_block
  in 
  match fun_def_with_own_appended with SemAST.FuncDef f ->
  SemAST.FuncDef {
    f with 
    func_def_local=my_new_locals; 
    func_def_block=my_new_body
  }
    

