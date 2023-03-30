type expr  =    | ()
                | Bool of bool
                | Int of int
                | String of string 
                | Var of char
                | Float of float
                | BinExpr of binop*expr*expr 
                | BinAssign of assignop*expr*expr
                | UnaryExpr of unaryop*expr
                | UnaryAssign of unaryassignop*expr
                | FuncCall of string*(expr list) (*Check again to see if it matches symbol table actions*)
                | Array of expr*expr
                | InlineIf of expr*expr*expr

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
                    | If of {cond:expr; ifstmt:statement ; elsestmt:statement}
                    | For of {label:string; initial: expr; cond: expr; update: expr; stmt: statement}
                    | JumpStmt of {name:jumpname; label: string}
                    | Return of expr

and jumpname    =   | Break 
                    | Continue


type declaration =  | DeclList of declaration list
                    | FuncDef of {name:string; parameters:string list ; body: DecList StmtList} (*function definition or declaration AND CHECK SYMBOL TABLE TO ORTHODOKSO*)
                    | FuncDecl of {name:string; parameters:string list}
                    | VarDeclaration of {name:string; size:int}