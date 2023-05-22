open Types
type loc = Lexing.position 

(* An identifer for a type, proc or variable *)

type ident = string

type pass_mode = PASS_BY_VALUE | PASS_BY_REFERENCE


(*typ is of type string e.g TBool/TInt followed by a string list that implies something like int ** (asterisks) *)


type expr  =    | ()
                | Ident of ident
                | Bool of bool
                | Int of int
                | Char of char
                | Float of float
                | BinExpr of binop*expr*expr 
                | BinAssign of assignop*expr*expr
                | UnaryExpr of unaryop*expr
                | UnaryAssign of unaryassignop*expr
                | Array of expr*expr
                | InlineIf of expr*expr*expr
                | FuncCall of func_call
and func_call = {name:string; parameters:string list}

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
                    | StmtList of loc * statement list (*maybe declare statement as stmt list ???? problem with traversing the list each time*)
                    | Expr of loc * expr
                    | If of loc * if_expr 
                    | For of loc * for_loop 
                    | JumpStmt of loc*jump
                    | Return of loc * expr
                    


and jumpname = 
             |Break
             |Continue

and jump =  {
             name:jumpname; 
             label_jump: string
             }
            
and if_expr = {
               cond:expr;
               ifstmt:statement; 
               elsestmt:statement
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
                 name:string; 
                 parameters:pass_mode*Types.typ*ident list;
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

and declaration =  | DeclList of loc * declaration list
                    | FuncDef of loc * func_def
                    | FuncDecl of loc * func_decl
                    | VarDeclaration of loc * var_decl 
                                     