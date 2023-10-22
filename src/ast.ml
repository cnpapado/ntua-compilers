open Types
open Symbol
(* open Printf *)

exception ConstantEvaluationError

type loc = Lexing.position 

type identifier = string
type label = string

type expr = | Int of int
            | Char of char  
            | Id of identifier 
            | String of string
            | Lvalue of lval
            | ExprFuncCall of func_call
            | SignedExpr of uop * expr
            | BinExpr of arithmetic_binop * expr * expr 
and lval = LvalueId of identifier | LvalueString of string | LvalueArr of lval * expr
and arithmetic_binop = Times | Div | Mod | Plus | Minus
and uop = UPlus | UMinus
and func_call = FuncCall of {name:string; parameters:expr list}

type cond = | ExprCond of comparison_binop * expr * expr
            | CompoundCond of logical_binop * cond * cond
            | NegatedCond of cond

and comparison_binop = Lt | Gt | Le | Ge | Eq | Neq 
and logical_binop = And | Or
        
type stmt = | EmptyStmt
            | Assign of {lvalue:lval; rvalue:expr}
            | Block of stmt list
            | StmtFuncCall of func_call
            | If of if_expr
            | While of while_expr
            | Return of expr option

and if_expr = {
  if_cond: cond;
  ifstmt: stmt; 
  elsestmt: stmt option
}

and while_expr = {
  while_cond: cond;
  whilestmt: stmt
}

type func_def = {
  func_def_header: header; 
  func_def_local: local_def list; 
  func_def_block: stmt list;
} 

                
and var_def = {
  var_def_id: identifier list;
  var_def_ret: Types.typ; 
}

and header = Header of {
  header_id: identifier;
  header_defs: (pass_mode * (identifier list) * typ) list; 
  header_ret: Types.typ option;
}

and func_decl = {
  func_decl_header: header;
}

and local_def = | FuncDef of func_def
                | FuncDecl of func_decl
                | VarDef of var_def

and ast_root = Root of func_def