(* open Types *)
(* open Symbol *)
(* open Printf *)

exception ConstantEvaluationError

type loc = Lexing.position 

type identifier = string
type label = string

type expr = | Int of int
            | Char of char  
            | Id of identifier 
            | String of string
            | Lvalue of {lvalue:lval; brackets:expr}
            | FuncCall of {name:string; parameters:expr list}
            | SignedExpr of uop * expr
            | BinExpr of arithmetic_binop * expr * expr 
and lval = Id of identifier | String of string | Lvalue
and arithmetic_binop = Times | Div | Mod | Plus | Minus
and uop = UPlus | UMinus

type cond = | ExprCond of comparison_binop * expr * expr
            | CompoundCond of logical_binop * cond * cond
            | NegatedCond of cond

and comparison_binop = Lt | Gt | Le | Ge | Eq | Neq 
and logical_binop = And | Or
           

(*** REMINANTS OF EDSGER BELOW: ***)

(* 
and assignop =      | Assign
                    | TimesEq
                    | DivEq
                    | ModEq 
                    | PlusEq
                    | MinusEq

and unaryop =       | BitAnd
                    | UTimes 
                    | UPlus
                    | UMinus 
                    | BitNot

and unaryassignop = | PrePlusPlus
                    | PostPlusPlus
                    | PreMinusMinus
                    | PostMinusMinus *)

(* 
type statement =    | EmptyStmt
                    | StmtList of statement list (*maybe declare statement as stmt list ???? problem with traversing the list each time*)
                    | Expr of expr
                    | If of if_expr 
                    | For of for_loop 
                    | Jump of {name: jumptype; label_jump: label}
                    | Return of expr
                    
and jumptype = Break | Continue

            
and if_expr = {
               if_cond: expr;
               ifstmt: statement; 
               elsestmt: statement
               }

and for_loop = {
                  label : identifier;
                  initial : expr;
                  for_cond : expr;
                  update : expr;
                  stmt : statement;
                }              


type func_def = {
                    func_def_return_type: Types.typ; 
                    func_def_name: identifier; 
                    func_def_parameters: (pass_mode*Types.typ*identifier) list;
                    func_def_body: (declaration list * statement list)
                } 

and func_decl = {
                    func_decl_return_type: Types.typ; 
                    func_decl_name: identifier; 
                    func_decl_parameters: (pass_mode*Types.typ*identifier) list
                 }
                
and var_decl = {
                var_decl_typ: Types.typ;
                var_decl_name: identifier 
               }

and declaration =  | DeclList of declaration list (* not needed? *)
                   | FuncDef of func_def
                   | FuncDecl of func_decl
                   | VarDeclaration of var_decl 

and const_expr = ConstExpr of expr | ConstOne 
(* ConstOne is used when no const_expr is present in the variable 
 * declaration, meaning that a single var has been declared and not an array
 * (in the declarator to be presice) to signify the higher 
 * layers of the AST (the variable decl to be precise) that the declared 
 * variable is not an array *)


let eval_const const = 
  let rec eval_aux e = 
  match e with 
  | Int(x) -> x
  | BinExpr(Times, e1, e2) -> (eval_aux e1) * (eval_aux e2)
  | BinExpr(Div, e1, e2) -> (eval_aux e1) / (eval_aux e2)
  | BinExpr(Mod, e1, e2) -> (eval_aux e1) mod (eval_aux e2)
  | BinExpr(Plus, e1, e2) -> (eval_aux e1) + (eval_aux e2)
  | BinExpr(Minus, e1, e2) -> (eval_aux e1) - (eval_aux e2)
  | BinExpr(And, e1, e2) -> (eval_aux e1) land (eval_aux e2)
  | BinExpr(Or, e1, e2) -> (eval_aux e1) lor (eval_aux e2)
  | _ -> raise ConstantEvaluationError
  
  in 
  match const with ConstExpr(e) -> eval_aux e


(* | InlineIf of {cond: expr; true_expr: expr; false_expr: expr} *)
| TypeCast of {new_type: Types.typ; casted_expr: expr} *)