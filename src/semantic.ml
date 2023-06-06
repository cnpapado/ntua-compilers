open Core
open Error
open Identifier
open Types
open Symbol
open Symbtest
open PrettyPrint
open TypeExpr

exception Found_but_different_typ


(* for type inference we need to dfs the ast? *)


let rec check_decl_list l = 
match l with
| Ast.VarDeclaration(x)::tl -> 
    Printf.printf "declared var \"%s\"\n" x.var_decl_name;
    (* add to symbol table *) 
    ignore (newVariable (id_make x.var_decl_name) x.var_decl_typ true);
    check_decl_list tl
| Ast.FuncDecl(x)::tl -> 
    Printf.printf "declared function \"%s\"\n" x.func_decl_name ; 
    (* add to symbol table *)
    ignore (newFunction (id_make x.func_decl_name) x.func_decl_return_type true);
    check_decl_list tl
| Ast.FuncDef(x)::tl -> 
    Printf.printf "defined function \"%s\"\n" x.func_def_name ;
    (* search for the declaration in the current scope only *)
    let f_entry = (lookupEntry (id_make x.func_def_name) ENTRY_TYPE_function LOOKUP_CURRENT_SCOPE true) in 
    (* TODO: make sure both argument list and return type match *)
    
    openScope (); 
    (* add the arguments to the current scope so they can be resolved in the function body *)
    (* TODO: handle arrays, pointers, etc *)
    (* TODO: handle arrays as pointers *)
    let add_args_to_scope args = 
    args |> List.rev    (* this could have been omitted if we weren't reversing in ast *)
    |> List.map ~f:(fun (mode,typ,id) -> 
        newParameter (id_make id) typ mode f_entry true) in

    ignore (add_args_to_scope x.func_def_parameters);
    
    (* TODO: (fix) check previously defined... *)
    check_func_body x.func_def_body;
    printSymbolTable ();
    closeScope ();
    check_decl_list tl
| [] -> print_string "done checking decl\n"; () 


and check_func_body b = 
     match b with (decl_l, stmt_l) -> 
         check_decl_list decl_l;
         check_stmt_list stmt_l


and check_stmt_list stmt_l = 
    match stmt_l with 
    | EmptyStmt::tl -> 
        Printf.printf "empty stmt\n";
        check_stmt_list tl
    | StmtList(stmt_l)::tl ->
        Printf.printf "statement list\n";
        (* TODO: check that scopes open only on functions *)
        check_stmt_list stmt_l;
    
        check_stmt_list tl
    | Expr(e)::tl -> 
        Printf.printf "expr\n";
        ignore (string_of_type (check_expr e));
        check_stmt_list tl
    | If({ if_cond=cond_expr; ifstmt=if_part; elsestmt=else_part })::tl ->
        Printf.printf "if\n";
        (* check condition evaluates to bool *)
    
        check_stmt_list [if_part];
        check_stmt_list [else_part];
        check_stmt_list tl
    | For({ label=_; initial=init_expr; for_cond=cond_expr; update=upd_expr; stmt=statement; })::tl ->
        Printf.printf "for\n";
        (* check for_cond evaluates to bool + other checks for initial & update ?*)
        (* Add label to symbol table *)
        ignore (check_expr init_expr);
        ignore (check_expr upd_expr);
        ignore (check_stmt_list [statement]);
    
        check_stmt_list tl
    | Jump(_)::tl ->
        Printf.printf "jump\n";
        (* check label exists *)
        check_stmt_list tl
    | Return(_)::tl -> 
        Printf.printf "return\n";
        (* check that return value agrees with function's return type *)
        check_stmt_list tl
    | [] -> print_string "done checking stmt\n"; () 
    

(*In the first place, this function will return the 
the type of each expression recursively *)



and check_expr e = 
match e with
| EmptyExpr -> 
    Printf.printf "empty expr\n";
    TYPE_void
| Id(id) -> 
    Printf.printf "identifier\n";
    (* we need to look up whether this identifier exists
       in the Symb Table and the proceed to return its saved type*)
    type_expr id ENTRY_TYPE_variable LOOKUP_CURRENT_SCOPE
    
| Bool(_) -> 
    Printf.printf "bool\n";
    TYPE_bool;
| Int(_) -> 
    Printf.printf "int\n";
    TYPE_int;
| Char(_) -> 
    Printf.printf "char\n";
    TYPE_char;
| Float(_) -> 
    Printf.printf "float\n";
    TYPE_double;
| String(_) -> 
    Printf.printf "id\n";
    TYPE_array {ttype = TYPE_char; size = 1}
| BinExpr(op,e1,e2) -> 
    Printf.printf "binExpr\n";
    let t1 = check_expr e1 in
    let t2 = check_expr e2 in
        check_bin_operator op t1 t2 
    (* check types of both sides are equal *)
    (* push up the resulting type for inference?  *)
| BinAssign(_,e1,e2) -> 
    Printf.printf "binAssign\n";
    let t1 = check_expr e1 in
    let t2 = check_expr e2 in
     Printf.printf "%s\n" (string_of_type t1);
     Printf.printf "%s\n" (string_of_type t2);
     if(equalType t1 t2) then t1 else (ignore(Printf.printf "Expected different types arithmetic op\n"); TYPE_none)
    (* check lvalue is declared and rvalue has correct type *)
| UnaryExpr(op,e) -> 
    Printf.printf "unaryExpr\n";
    let t = check_expr e in
        check_unary_operator op t 
    (* check unary op can be performed to this expr *)
    (* push up the resulting type for inference?  *)
| UnaryAssign(op,e) -> 
    Printf.printf "unaryAssign\n";
    let t = check_expr e in
        sign_type t 
    (* check unary op can be performed to this expr *)
    (* push up the resulting type for inference?  *)
| Array {name=e1; size=e2} -> 
    Printf.printf "array\n";
    let t1 = check_expr e1 in
    let t2 = check_expr e2 in
        let f t1 t2 = 
             match t2 with
                 |TYPE_int -> t1
                 |_ -> (ignore(error "size of array not returning an integer"); TYPE_none)
        in f t1 t2
    (* check name is identifier and size evaluates to int? *)
| InlineIf {cond=e; true_expr=e1; false_expr=e2} -> 
    Printf.printf "inlineIf\n";
    let t1 = check_expr e1 in
    let t2 = check_expr e2 in
    let t  = check_expr e  in
      let g t t1 t2 = 
         match t with
         |TYPE_bool -> 
            let f t1 t2 = 
                if(equalType t1 t2) then t1 else (ignore(error "Types not equal in if expr\n");TYPE_none)
                in  f t1 t2
        | _ -> (ignore(error "First condition not a bool in if expr\n");TYPE_none)
        in g t t1 t2
    (* check cond=bool, return types the same *)
| FuncCall {name=n; parameters = e_lst} -> 
    Printf.printf "funcCall\n";
    (* let f_entry = lookupEntry (id_make n) ENTRY_TYPE_function LOOKUP_CURRENT_SCOPE true in
    let check_params p = 
        p |> List.rev    (* this could have been omitted if we weren't reversing in ast *)
        |> List.map ~f:(fun (mode,typ,id) -> 
            newParameter (id_make id) typ mode f_entry true) in
    
        ignore (add_args_to_scope x.func_def_parameters);
     *)
    (* check correct parameters, previously declared *)
    TYPE_none;
| Delete e -> 
    Printf.printf "del\n";
    let t = check_expr e in
    let f t = 
        match t with
        |TYPE_ptr {ttype= tt; level = _} -> TYPE_ptr {ttype = t; level=0}
        |_ -> Printf.printf "delete expr requires t* \n"; TYPE_none
    in f t
    (* delete something that was declared + is an identifier/array *)
| New {ttype=t; size=e} -> 
    Printf.printf "new\n";
    let t = check_expr e in
       let f s = 
        match s with 
         |TYPE_int -> 
           let g t =
            match t with
            | TYPE_ptr {ttype=_; level=l} -> l+1
            | _ -> 1
           in
           TYPE_ptr {ttype=t; level= (g t)}
        | _ -> Printf.printf "new size not an int\n"; TYPE_none
        in f t
    (* check size evaluates to int *)
(* | TypeCast (_) -> 
    Printf.printf "type cast\n";
    TYPE_none;
   *)