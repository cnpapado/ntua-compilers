open Types
open Symbol



(** Encapsulates the type of an ast node *)
module type Node = sig type t end


(** The grammar of all ASTs, parameterized by the node type *)
module MakeAST (Node : Node) = struct
  type loc = Lexing.position 

  type identifier = string
  type label = string
  
  type expr = | Int of {i:int; meta: Node.t}
              | Char of {c:char; meta: Node.t}  
              | Id of {id:identifier; meta: Node.t}  (*not used*)
              | String of {s:string; meta: Node.t}   (*not used*)
              | Lvalue of lval
              | ExprFuncCall of func_call
              | SignedExpr of {sign:uop; e:expr; meta: Node.t}
              | BinExpr of {l:expr; r:expr; op: arithmetic_binop; meta: Node.t} 
  and lval = LvalueId of {id:identifier; meta: Node.t} | LvalueString of {s:string; meta: Node.t} | LvalueArr of {arr:lval * expr; meta: Node.t}
  and arithmetic_binop = Times | Div | Mod | Plus | Minus
  and uop = UPlus | UMinus
  and func_call = FuncCall of {name:string; parameters:expr list; meta: Node.t}
  
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
              | Return of expr option
  
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
    header_fpar_defs: (pass_mode * identifier * typ) list; 
    header_ret: Types.typ option;
    meta: Node.t
  }
  
  (* and func_decl = {
    func_decl_header: header;
  } *)
  
  and local_def = | FuncDef of func_def
                  | FuncDecl of header
                  | VarDef of var_def
  
  and ast_root = Root of func_def

end

(* An AST with location info *)
module ParserAST = struct 
  type node_with_loc_info = int (* {fname: string; line_no: int; char_no: int} *)
  include MakeAST (struct type t = node_with_loc_info end)
end


(* An AST with type info *)
module SemAST = struct 
  type node_with_type_info = {typ: int} 
  include MakeAST (struct type t = node_with_type_info end)
end


















