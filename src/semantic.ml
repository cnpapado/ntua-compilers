open Core

open Identifier
open Types
open Symbol
open Symbtest

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
    check_expr e;
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
    check_expr init_expr;
    check_expr upd_expr;
    check_stmt_list [statement];

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


and check_expr e = 
match e with
| EmptyExpr -> 
    Printf.printf "empty expr\n"
| Id(_) -> 
    Printf.printf "identifier\n"
    (* ????? *)
| Bool(_) -> 
    Printf.printf "bool\n"
| Int(_) -> 
    Printf.printf "int\n"
| Char(_) -> 
    Printf.printf "char\n"
| Float(_) -> 
    Printf.printf "float\n"
| String(_) -> 
    Printf.printf "id\n"
| BinExpr(_) -> 
    Printf.printf "binExpr\n"
    (* check types of both sides are equal *)
    (* push up the resulting type for inference?  *)
| BinAssign(_) -> 
    Printf.printf "binAssign\n"
    (* check lvalue is declared and rvalue has correct type *)
| UnaryExpr(_) -> 
    Printf.printf "unaryExpr\n"
    (* check unary op can be performed to this expr *)
    (* push up the resulting type for inference?  *)
| UnaryAssign(_) -> 
    Printf.printf "unaryAssign\n"
    (* check unary op can be performed to this expr *)
    (* push up the resulting type for inference?  *)
| Array(_) -> 
    Printf.printf "array\n"
    (* check name is identifier and size evaluates to int? *)
| InlineIf(_) -> 
    Printf.printf "inlineIf\n"
    (* check cond=bool, return types the same *)
| FuncCall(_) -> 
    Printf.printf "funcCall\n"
    (* check correct parameters, previously declared *)
| Delete(_) -> 
    Printf.printf "del\n"
    (* delete something that was declared + is an identifier/array *)
| New(_) -> 
    Printf.printf "new\n"
    (* check size evaluates to int *)
| TypeCast(_) -> 
    Printf.printf "type cast\n"
    (* check expr is something that can be type casted + size=int *)