open Ast


(* Return a list of the free variables of a function definition *)
let free_vars (SemAST.FuncDef def) = 
  let params = 
    List.map (fun (_,id,_) -> id) (match def.func_def_header with Header h -> h.header_fpar_defs) in
  let locals =
    List.concat @@ List.map
    (fun ldef -> match ldef with
      | SemAST.FuncDef _ 
      | SemAST.FuncDecl _ -> []
      | SemAST.VarDef v -> v.var_def_id)
    def.func_def_local
  in 
  let referenced = (* traverse and append all ids to list *) 
    (* return l appended with all ids found in stmt *)
    let rec collect_stmt l stmt = 
      match stmt with 
      | SemAST.EmptyStmt -> l
      | SemAST.Assign {lvalue; rvalue; meta=_} -> List.concat [l; (collect_lval l lvalue); (collect_expr [] rvalue)]
      | SemAST.Block stmt_list -> List.fold_left collect_stmt l stmt_list (* collect_stmt (collect_stmt (collect_stmt l b0) b1) b2 *)
      | SemAST.StmtFuncCall f -> (match f with SemAST.FuncCall ff -> ff.name) :: l
      | SemAST.If i -> List.concat [(collect_cond l i.if_cond); (collect_stmt [] i.ifstmt); (match i.elsestmt with Some e -> collect_stmt [] e | None -> [])]
      | SemAST.While w -> List.concat [(collect_cond l w.while_cond); (collect_stmt l w.whilestmt)]
      | SemAST.Return {ret; meta=_} -> match ret with Some sr -> collect_expr l sr | None -> l
    and collect_lval l lval =
      match lval with 
      | LvalueId {id; meta=_} -> id :: l 
      | LvalueString _ -> l
      | LvalueArr {arr=(lval_arr, idx_expr); meta=_} -> (collect_lval l lval_arr) @ (collect_expr [] idx_expr)
    and collect_expr l e = 
      match e with
      | SemAST.Int i -> l
      | SemAST.Char c -> l
      | SemAST.Lvalue lval -> collect_lval l lval
      | SemAST.ExprFuncCall f -> (match f with SemAST.FuncCall ff -> ff.name) :: l
      | SemAST.SignedExpr se -> collect_expr l se.e
      | SemAST.BinExpr b -> (collect_expr l b.l) @ (collect_expr [] b.r)
    and collect_cond l c = match c with 
      | SemAST.ExprCond c -> (collect_expr l c.l) @ (collect_expr [] c.r)
      | SemAST.CompoundCond c -> (collect_cond l c.l) @ (collect_cond [] c.r)
      | SemAST.NegatedCond c -> collect_cond l c
    in 
    collect_stmt [] def.func_def_block
  in
    (* Printf.printf "\n\n%s" "Bound:";
    List.iter (Printf.printf "%s, ") @@ params @ locals;
    Printf.printf "\n\n%s" "Referenced:";
    List.iter (Printf.printf "%s, ") referenced;
    Printf.printf "\n\n%s" ""; *)
    let diff l1 l2 = List.filter (fun x -> not (List.mem x l2) && not (Rename.SS.mem x !Rename.function_names)) l1 in
    (* Printf.printf "\n\n%s" "Free:"; *) 
    let free = diff referenced (params @ locals) in
    (* List.iter (Printf.printf "%s, ") free; *)
    free

let replace_free root =
  free_vars root