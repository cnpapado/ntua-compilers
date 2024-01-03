open Ast
open Rename
open Replace

(* return a tuple of a new ast without the functions which have no free vars
    and a list with the functions removed *)
let rec remove_independent ast = 
  let (head, locals, body, meta) = match ast with SemAST.FuncDef d -> 
    (d.func_def_header, d.func_def_local, d.func_def_block, d.meta) in

  (* apply recursivelly to my locals... *)
  let (stripped_locals, inner_independent_defs) = 
    (* for each local_def strip it's own locals from indep funs and gather them *)
    List.split @@ List.map (fun d -> match d with 
    | SemAST.FuncDef _ -> remove_independent d
    | _ -> (d, [])) locals
  in
  let is_indep_def d = 
    match d with
    | SemAST.FuncDef _ -> 
      begin
        match free_vars d with 
        | [] -> true
        | _ -> false
      end
    | _ -> false
  in 
  let is_indep_decl indep_defs d =
    match d with 
    | SemAST.FuncDecl decl -> 
      (* search if it appears in indep_defs *)
        let my_name = (match decl with SemAST.Header h -> h.header_id) in
        let get_name (SemAST.FuncDef def) = (match def.func_def_header with SemAST.Header h -> h.header_id) in
        let indep_defs_ids = List.map get_name indep_defs in
        List.exists (fun x -> x=my_name) indep_defs_ids
    | _ -> false
  in
  (* first gather indepependent definitions *)
  let (independent_defs, remaining_locals) = List.partition is_indep_def stripped_locals in
  (* and then gather indepependent declarations *)
  let (independent_decls, remaining_locals) = List.partition (is_indep_decl independent_defs) remaining_locals in

  let new_ast = 
  SemAST.FuncDef {
    func_def_header=head; 
    func_def_local=remaining_locals; 
    func_def_block=body;
    meta=meta
  } in
  (new_ast, List.concat inner_independent_defs @ independent_decls @ independent_defs)
  


(* return a new ast with all decls/defs which have no free vars being moved to global scope *)  
let llift root =
  let unique_ast = rename_ast root in

  (* takes an ast and keeps replacing and extracting indep until no more funcs can be extracted 
     returns a tuple with the modified ast and the list of extracted funcs *)
  let rec keep_replacing_and_extracting ast prev_extra = 
    let _ = Hashtbl.clear Replace.free_vars_hashtbl; Printf.printf "pass-input ast:%s" "\n"; ((Printf.printf "%s, ") (Pretty_print.str_of_localdef ast)) in
    let after_step2 = replace_free ast in
    let (after_step3, extra) = remove_independent after_step2 in
    match extra = prev_extra with 
    | true -> (after_step3, extra) (* converged *)
    | false -> 
      (let (new_ast, new_extra) = 
      Printf.printf "pass-extra fs:%s" "\n"; (List.iter (Printf.printf "%s, ") (List.map Pretty_print.str_of_localdef extra));
      keep_replacing_and_extracting after_step3 extra in
      (new_ast, new_extra @ extra))
      
  in
  match (keep_replacing_and_extracting unique_ast []) with (ast, moved_functions) -> 
    let main_call = 
      let main_name = (match ast with SemAST.FuncDef def -> (match def.func_def_header with SemAST.Header h -> h.header_id)) in
      SemAST.FuncCall {name=main_name; parameters=[]; meta={typ=Some Types.TYPE_nothing}}
    in
    SemAST.FuncDef {
      func_def_header=SemAST.Header { 
        header_id="global_sco";
        header_fpar_defs=[]; 
        header_ret=Types.TYPE_uninitialized;
        meta={typ=Some Types.TYPE_uninitialized}
      }; 
      func_def_local=moved_functions @ [ast]; 
      func_def_block=SemAST.Block [SemAST.StmtFuncCall main_call]; 
      meta={typ=Some Types.TYPE_uninitialized}
    } 
    