open Types
(* open Symbol *)
exception InternalAstUtilsError of string

(** Encapsulates the type of an ast node *)
module type Node = sig type t end


(** The grammar of all ASTs, parameterized by the node type *)
module MakeAST (Node : Node) = struct
  type loc = Lexing.position 

  type identifier = string
  type label = string
  
  type expr = | Int of {i:int; meta: Node.t}
              | Char of {c:char; meta: Node.t}  
              (* | Id of {id:identifier; meta: Node.t}
              | String of {s:string; meta: Node.t}  *)
              | Lvalue of lval
              | ExprFuncCall of func_call
              | SignedExpr of {sign:uop; e:expr; meta: Node.t}
              | BinExpr of {l:expr; r:expr; op: arithmetic_binop; meta: Node.t} 
  and lval = LvalueId of {id:identifier; meta: Node.t} | LvalueString of {s:string; meta: Node.t} | LvalueArr of {arr:lval * expr; meta: Node.t}
  and arithmetic_binop = Times | Div | Mod | Plus | Minus
  and uop = UPlus | UMinus
  and func_call = FuncCall of {name:identifier; parameters:expr list; meta: Node.t}
  
  type cond = | ExprCond of {l:expr; r:expr; op: comparison_binop; meta: Node.t} 
              | CompoundCond of {l:cond; r:cond; op: logical_binop; meta: Node.t} 
              | NegatedCond of cond
  
  and comparison_binop = Lt | Gt | Le | Ge | Eq | Neq 
  and logical_binop = And | Or
          
  type stmt = | EmptyStmt
              | Assign of {lvalue:lval; rvalue:expr; meta: Node.t}
              | Block of (stmt list)
              | StmtFuncCall of func_call
              | If of if_expr
              | While of while_expr
              | Return of {ret:expr option; meta: Node.t}
  
  and if_expr = {
    if_cond: cond;
    ifstmt: stmt; 
    elsestmt: stmt option;
    meta: Node.t
  }
  
  and while_expr = {
    while_cond: cond;
    whilestmt: stmt;
    meta: Node.t
  }
  
  type func_def = {
    func_def_header: header; 
    func_def_local: local_def list; 
    func_def_block: stmt;
    meta: Node.t
  } 
  
                  
  and var_def = {
    var_def_id: identifier list;
    var_def_typ: Types.typ;
    meta: Node.t
  }
  
  and header = Header of {
    header_id: identifier;
    header_fpar_defs: (Types.pass_mode * identifier * Types.typ) list; 
    header_ret: Types.typ;
    meta: Node.t
  }
  
  (* and func_decl = {(get_type sem_ap)
    func_decl_header: header;
  } *)
  
  and local_def = | FuncDef of func_def
                  | FuncDecl of header
                  | VarDef of var_def
  
  (* and def_or_decl = GlobalDef of func_def | GlobalDecl of header
  and global_scope = Program of def_or_decl list *)


  let rec str_of_lval2 = function
    | LvalueId {id; meta=_} -> id 
    | LvalueString {s; meta=_} -> "str(" ^ s ^ ")" 
    | LvalueArr {arr=(l,e); meta=_} -> (str_of_lval2 l) ^ "[...]"

  let rec get_arr_id = function
    | LvalueId {id; meta=_} -> id
    | LvalueString _ -> raise (InternalAstUtilsError "found str lvalue while getting id of array")
    | LvalueArr {arr=(lval, expr); meta=_} -> get_arr_id lval

  (* get the type of a (possibly imcomplete array)*)
  let arr_typ lval_arr get_id_typ =  
    
    (* find the type this id has (type of the array as declared)
       because in this reference, some dims might be missing (if incomplete) *)
    let whole_arr_type = get_id_typ (get_arr_id lval_arr) in
    let whole_dim = arr_dim whole_arr_type in
    let curr_dim = 
      let rec dim a n = 
        match a with 
        | LvalueArr {arr=(inner_arr, _); meta=_} -> dim inner_arr n+1 
        | LvalueId _ -> n 
        | _ -> raise (InternalAstUtilsError ("found str lval inside an arr"))
      in dim lval_arr 0
    in
    
    let rec strip_first (n, t) = 
      if n = 0 then t 
      else
        match t with 
        | TYPE_array {ttype; size=_} -> strip_first (n-1, ttype)
        | _ -> raise (InternalAstUtilsError ("reached non arr sooner than 0 nest level"))
    in 
    
    (* Printf.printf "whole arr dim= %d, curr dim= %d\n" whole_dim curr_dim; *)
    if whole_dim = curr_dim then get_base_arr_typ whole_arr_type
    else strip_first (whole_dim-curr_dim, whole_arr_type)  

end

(* An AST with location info *)
module ParserAST = struct 
  type node_with_loc_info = Lexing.position  (* {line_no: int; col_no: int} *)
  include MakeAST (struct type t = node_with_loc_info end)
end


(* An AST with type info *)
module SemAST = struct 
  type node_with_type_info = {typ: Types.typ option} 
  include MakeAST (struct type t = node_with_type_info end)
end


















