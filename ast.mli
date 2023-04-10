type loc = Lexing.position 

(* An identifer for a type, proc or variable *)

type ident = string

type pass_mode = PASS_BY_VALUE | PASS_BY_REFERENCE


(*typ is of type string e.g TBool/TInt followed by a string list that implies something like int ** (asterisks) *)


type expr  =    | Unit of loc
                | Ident of loc * ident
                | Bool of loc * bool
                | Int of loc * int
                | Char of loc * char
                | Float of loc * float
                | BinExpr of loc*binop*expr*expr 
                | BinAssign of loc*assignop*expr*expr
                | UnaryExpr of loc*unaryop*expr
                | UnaryAssign of loc*unaryassignop*expr
                | FuncCall of loc * func_call
                           and func_call = {name:string; parameters:string list}
                | Array of loc*expr*expr
                | InlineIf of loc*expr*expr*expr

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
                         and if_expr = {cond:expr; ifstmt:statement ; elsestmt:statement}
                    | For of loc * for_loop 
                          and for_loop = {label:string; initial: expr; cond: expr; update: expr; stmt: statement}
                    | JumpStmt of loc*jump
                               and jump = {name:jumpname; label: string}
                    | Return of loc * expr

and jumpname    =   | Break 
                    | Continue


type declaration =  | DeclList of loc * declaration list
                    | FuncDef of loc * func_def
                              and func_def = {typ: Types.typ; name:string; parameters:pass_mode*Types.typ*ident list ; body: (declaration list * statement list)} (*function definition or declaration AND CHECK SYMBOL TABLE TO ORTHODOKSO*)
                    | FuncDecl of loc * func_decl
                               and func_decl = {typ: Types.typ; name:string; parameters:pass_mode*Types.typ*ident list} 
                    | VarDeclaration of loc * var_decl 
                                     and var_decl = {typ:Types.typ ;name: ident ; size:int}