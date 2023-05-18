open Types
type loc = Lexing.position 

(* An identifer for a type, proc or variable *)

and ident = string

and pass_mode = PASS_BY_VALUE | PASS_BY_REFERENCE


(*typ is of type string e.g TBool/TInt followed by a string list that implies something like int ** (asterisks) *)

and expr  =     | ()
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
                | New of {ttype: Types.typ; size: expr}
                | TypeCast of {new_type: Types.typ; casted_expr: expr}
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

and declaration =   | DeclList of declaration list
                    | FuncDef of func_def
                    | FuncDecl of func_decl
                    | VarDeclaration of var_decl 

let string_of_loc loc = 
  "line " ^ (string_of_int (loc.Lexing.pos_lnum)) ^ ", " ^ 
  "position " ^ (string_of_int ((loc.Lexing.pos_cnum - loc.Lexing.pos_bol) + 1))

(*function to print type in string format*)
let rec pp_type = function 
    | TYPE_int  -> "int" 
    | TYPE_char -> "char" 
    | TYPE_bool -> "bool"
    | TYPE_double -> "double" 
    | TYPE_array {ttype = t; size = sz} -> "Array"

(*function to print unary operators in string format*)

let pp_uop = function 
    | BitAnd     -> "&"
    | UTimes     -> "*"
    | UPlus      -> "+"
    | UMinus     -> "-"
    | BitNot     -> "!"

let pp_assop = function
    | Assign   -> "="
    | TimesEq  -> "+="
    | DivEq    -> "/="
    | ModEq    -> "%="
    | PlusEq   -> "+="
    | MinusEq  -> "-="

let pp_unassop = function
    | PostPlusPlus      -> "++"
    | PrePlusPlus       -> "++"
    | PreMinusMinus     -> "--"
    | PostMinusMinus     -> "--"


(*function to print binary operators in string format*)
let pp_bop = function
    | Times   -> "*"
    | Div     -> "/"
    | Mod     -> "%"
    | Plus    -> "+"
    | Minus   -> "-"
    | Lt      -> "<"
    | Gt      -> ">"
    | Le      -> "<="
    | Ge      -> ">="
    | Eq      -> "=="
    | Neq     -> "!="
    | And     -> "&&"
    | Or      -> "||"
    | Comma   -> ","


let mk_con con l = 
    let rec aux carry = function 
      | [] -> carry ^ ")"
      | [s] -> carry ^ s ^ ")"
      | s::rest -> aux (carry ^ s ^ ", ") rest 
    in aux (con ^ "(") l 

let rec string_of_expr e = match e with 
    | ()                                    -> "Unit"
    | Ident       s                         -> make_con "Ident" s
    | Bool        b                         -> make_con "Bool" (string_of_bool b)
    | Int         x                         -> make_con "Int" (string_of_int x)
    | Char        x                         -> make_con "Char" x
    | Float       x                         -> make_con "Float" (string_of_float x)
    | BinExpr     (op,e1,e2)                -> make_con "BinExpr" [string_of_expr e1 pp_bop op string_of_expr e2]
    | BinAssign   (op,e1,e2)                -> make_con "BinAssign" [string_of_expr e1 pp_assop op string_of_expr e2]
    | UnaryExpr   (op,e)                    -> make_con "UnaryExpr" [pp_bop pp_assop string_of_expr e]
    | UnaryAssign (op,e)                    -> make_con "UnaryAssign" [pp_bop pp_unassop string_of_expr e]
    | FuncCall    {name:n;parameters: p}    -> make_con "FuncCallname" [n ; 
                                                                          mk_con "" p
                                                                          ]
    | Array (e1,e2)                         -> make_con "Array" [e1; mk_con "" [e2]]
    | InlineIf (e1,e2,e3)                   -> make_con "InlineIf" [string_of_expr e1; string_of_expr e2;
                                                                      string_of_expr e3]

let string_of_jumpname = function
    | Break    -> "Break"
    | Continue -> "Continue"

let rec string_of_stmt = function
        | StmtList (_, Stlist)                             -> make_con "Stmnt" [string_of_stmt_list StList]
        | Expr (_,e)                                       -> make_con "Expr" [e]
        | If   (_,{cond:e; ifstmt:stmt1 ; elsestmt:stmt2}) -> make_con "If" [e;stmt1;stmt2]
        | For  (_,{label:l; initial:e1; cond:e2; update:e3; stmt: stmt})
                                                           -> make_con "For" [l;
                                                                              string_of_expr e1;
                                                                              string_of_expr e2;
                                                                              string_of_expr e3;
                                                                              string_of_stmt stmt;
                                                                              ]
        | JumpStmt (_,{name:jmp; label:l})                 -> make_con "JumpStmt" [string_of_jumpname jmp;
                                                                               l]
        | Return (_,e)                                     -> make_con "Return" [string_of_expr e]

    and string_of_stmt_list = function 
    | [] -> "" 
    | [e] -> string_of_expr e 
    |  e:: rest -> (string_of_expr e ) ^ "; " ^ (string_of_expr_list rest)
