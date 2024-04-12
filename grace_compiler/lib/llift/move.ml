open Ast
open Rename
open Replace

(* return i) a tuple of a new ast without the functions which have no free vars
    ii) a list with the functions (defs) removed iii) a list with decls removed *)
let rec remove_independent ast = 
  let (head, locals, body, meta) = match ast with SemAST.FuncDef d -> 
    (d.func_def_header, d.func_def_local, d.func_def_block, d.meta) in

  (* apply recursivelly to my locals... *)
  let (stripped_locals, (inner_independent_defs, inner_independent_decls)) = 
    (* for each local_def strip it's own locals from indep funs and gather them *)
  
    (* let list_of_triples_to_triple_of_lists lst =
      let split3 lst =
        let rec aux acc1 acc2 acc3 = function
          | [] -> (List.rev acc1, List.rev acc2, List.rev acc3)
          | (x, y, z)::t -> aux (x :: acc1) (y :: acc2) (z :: acc3) t
        in
        aux [] [] [] lst in
        let (firsts, seconds, thirds) = split3 lst in
        (firsts, List.flatten seconds, thirds) in *)
    
    let (a,list_of_t) = List.split @@ List.map (fun d -> match d with 
    | SemAST.FuncDef _ -> remove_independent d
    | _ -> (d, ([], [])) ) locals in 
    let (a, (b,c)) = (a, List.split list_of_t) in
    let remove_empty_lists lst =
      List.filter (fun sublst -> sublst <> []) lst in
    (a, (List.flatten b, List.flatten c)) 

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
  (new_ast, (List.concat [inner_independent_defs; independent_defs],
    List.concat [independent_decls; inner_independent_decls; [SemAST.FuncDecl head]]) ) (* add a decl of parent func *)
  


(* let remove_duplicate_decls l = 
  let are_equal (SemAST.FuncDecl a, SemAST.FuncDecl b) =
    match (a, b) with (SemAST.Header aa, SemAST.Header bb) ->
      aa.header_id == bb.header_id
      aa.header_ret == bb.header_ret
 *)

(* return a new ast with all decls/defs which have no free vars being moved to global scope *)  
let llift root =
  let unique_ast = rename_ast root in

  (* let free = free_vars unique_ast in
  Printf.printf "%s:%s " "ROOT:" (match unique_ast with SemAST.FuncDef def -> (match def.func_def_header with Header h -> h.header_id)); 
  Printf.printf "free vars: %s" ""; List.iter (Printf.printf "%s ") (List.map (fun (x,y)->x) free); Printf.printf "%s" "\n";

  raise Exit;
  unique_ast *)



  (* takes an ast and keeps replacing and extracting indep until no more funcs can be extracted 
     returns a tuple with the modified ast and the list of extracted funcs *)
  let rec keep_replacing_and_extracting ast prev_extra_defs = 
    let _ = Hashtbl.clear Replace.free_vars_hashtbl; in
    (* Printf.printf "pass-input ast:%s" "\n"; ((Printf.printf "%s, ") (Pretty_print.str_of_localdef ast)) *)
    let after_step2 = replace_free ast in
    let (after_step3, (extra_defs, extra_decls)) = remove_independent after_step2 in
    match extra_defs = prev_extra_defs with 
    | true -> (after_step3, (extra_defs, extra_decls)) (* converged *)
    | false -> 
      (let (new_ast, (new_extra_defs, new_extra_decls)) = 
      (* Printf.printf "pass-extra fs:%s" "\n"; (List.iter (Printf.printf "%s, ") (List.map Pretty_print.str_of_localdef extra)); *)
      keep_replacing_and_extracting after_step3 extra_defs in
      (new_ast, (new_extra_defs @ extra_defs, new_extra_decls @ extra_decls)))
      
  in
  match (keep_replacing_and_extracting unique_ast []) with (ast, (moved_defs, moved_decls)) -> 
    let main_call = 
      let main_name = (match ast with SemAST.FuncDef def -> (match def.func_def_header with SemAST.Header h -> h.header_id)) in
      SemAST.FuncCall {name=main_name; parameters=[]; meta={typ=Some Types.TYPE_nothing}}
    in
    let rec remove_duplicates lst =
      let rec is_duplicate x = function
        | [] -> false
        | y :: ys -> if x = y then true else is_duplicate x ys
      in
      match lst with
      | [] -> [] 
      | x :: xs ->
        if is_duplicate x xs then
          remove_duplicates xs 
        else
          x :: (remove_duplicates xs) 
    in
    SemAST.FuncDef {
      func_def_header=SemAST.Header { 
        header_id="main";
        header_fpar_defs=[]; 
        header_ret=Types.TYPE_int;
        meta={typ=Some Types.TYPE_int}
      }; 
      func_def_local= (remove_duplicates moved_decls) @ moved_defs @ [ast]; 
      func_def_block=SemAST.Block [SemAST.StmtFuncCall main_call]; 
      meta={typ=Some Types.TYPE_int}
    } 
    