open Types
type loc = Lexing.position 

(* An identifierifer for a type, proc or variable *)

type identifier = string
type label = string

type pass_mode = PASS_BY_VALUE | PASS_BY_REFERENCE


(*typ is of type string e.g TBool/TInt followed by a string list that implies something like int ** (asterisks) *)


type expr  =    | ()
                | Id of identifier
                | Bool of bool
                | Int of int
                | Char of char
                | Float of float
                | String of string
                | BinExpr of binop*expr*expr 
                | BinAssign of assignop*expr*expr
                | UnaryExpr of unaryop*expr
                | UnaryAssign of unaryassignop*expr
                | Array of {name:expr; size:expr}
                | InlineIf of {cond: expr; true_expr: expr; false_expr: expr}
                | FuncCall of {name:string; parameters:expr list}
                | Delete of expr
                | New of {ttype: Types.typ; size: expr}
                | TypeCast of {new_type: Types.typ; casted_expr: expr}

and binop =    
               | Times
               | Div
               | Mod
               | Plus
               | Minus
               | Lt
               | Gt
               | Le
               | Ge
               | Eq
               | Neq
               | And
               | Or
               | Comma

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
                    | PostMinusMinus


type statement =    | ()
                    | StmtList of statement list (*maybe declare statement as stmt list ???? problem with traversing the list each time*)
                    | Expr of expr
                    | If of if_expr 
                    | For of for_loop 
                    | Jump of {name: jumptype; label_jump: label}
                    | Return of expr
                    
and jumptype = Break | Continue

            
and if_expr = {
               cond: expr;
               ifstmt: statement; 
               elsestmt: statement
               }

and for_loop = {
                  label : identifier;
                  initial : expr;
                  cond : expr;
                  update : expr;
                  stmt : statement;
                }              


type func_def = {
                 return_type: Types.typ; 
                 name: identifier; 
                 parameters: (pass_mode*Types.typ*identifier) list;
                 body: (declaration list * statement list)
                } 

and func_decl = {
                  return_type: Types.typ; 
                  name: identifier; 
                  parameters: (pass_mode*Types.typ*identifier) list
                 }
                 
and var_decl = {
                  typ: Types.typ;
                  name: identifier ; 
                  size: const_expr
            }

and declaration =  | DeclList of declaration list
                   | FuncDef of func_def
                   | FuncDecl of func_decl
                   | VarDeclaration of var_decl 

and const_expr = ConstExpr of expr | () (* ?? *)