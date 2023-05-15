open Types
type loc = Lexing.position 

(* An identifer for a type, proc or variable *)

and ident = string

and pass_mode = PASS_BY_VALUE | PASS_BY_REFERENCE


(*typ is of type string e.g TBool/TInt followed by a string list that implies something like int ** (asterisks) *)

and expr  =    | ()
                | Ident of ident
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
                | FuncCall of func_call
                | Delete of expr
                | New of {ttype: typ; size: expr}
                | TypeCast of {new_type: typ; casted_expr: expr}
and func_call = {name:string; parameters:expr list}

(*and typ = TYPE_none
| TYPE_int
| TYPE_bool
| TYPE_char
| TYPE_double
| TYPE_array of {ttype: typ; size: int}
| TYPE_ptr of {ttype: typ; level: int}*)

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
                    | JumpStmt of jump
                    | Return of expr
                    


and jumpname = 
             |Break
             |Continue

and jump =  {
             name: jumpname; 
             label_jump: string
             }
            
and if_expr = {
               cond: expr;
               ifstmt: statement; 
               elsestmt: statement
               }

and for_loop = {
                  label : string;
                  initial : expr;
                  cond : expr;
                  update : expr;
                  stmt : statement list;
                }              


type func_def = {
                 typ: Types.typ; 
                 name: string; 
                 parameters: pass_mode*Types.typ*ident list;
                 body: (declaration list * statement list)
                } (*function definition or declaration AND CHECK SYMBOL TABLE TO ORTHODOKSO*)

and func_decl = {
                  typ: Types.typ; 
                  name:string; 
                  parameters:pass_mode*Types.typ*ident list
                 }
                 
and var_decl = {
                  typ:Types.typ ;
                  name: ident ; 
                  size:int
            }

and declaration =  | DeclList of declaration list
                    | FuncDef of func_def
                    | FuncDecl of func_decl
                    | VarDeclaration of var_decl 

