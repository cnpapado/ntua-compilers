open Types
open Ast

let string_of_loc loc = 
    "line " ^ (string_of_int (loc.Lexing.pos_lnum)) ^ ", " ^ 
    "position " ^ (string_of_int ((loc.Lexing.pos_cnum - loc.Lexing.pos_bol) + 1))
  
  (*function to print type in string format*)
  let rec pp_type = function 
      | TYPE_none -> "this should never be printed"
      | TYPE_int  -> "int" 
      | TYPE_char -> "char" 
      | TYPE_bool -> "bool"
      | TYPE_double -> "double" 
      | TYPE_array {ttype = t; size = _} -> pp_type t ^ " array"
      | TYPE_ptr   {ttype=t; level = l} -> pp_type t ^ "pointer of level" ^ (string_of_int l)
  
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
  
  let rec string_of_expr_list = function 
      | [] -> "" 
      | [e] -> string_of_expr e 
      | e::rest -> (string_of_expr e ) ^ "; " ^ (string_of_expr_list rest)
  
  and string_of_expr e = match e with 
      | EmptyExpr                             -> "Unit"
      | Id          s                         -> mk_con "Ident" [s]
      | Bool        b                         -> mk_con "Bool" [(string_of_bool b)]
      | Int         x                         -> mk_con "Int" [(string_of_int x)]
      | Char        x                         -> mk_con "Char" [String.make 1 x]
      | Float       x                         -> mk_con "Float" [(string_of_float x)]
      | String      s                         -> mk_con "String" [s]
      | BinExpr     (op,e1,e2)                -> mk_con "BinExpr" [string_of_expr e1 ;pp_bop op; string_of_expr e2]
      | BinAssign   (op,e1,e2)                -> mk_con "BinAssign" [string_of_expr e1 ;pp_assop op ;string_of_expr e2]
      | UnaryExpr   (op,e)                    -> mk_con "UnaryExpr" [pp_uop op; string_of_expr e]
      | UnaryAssign (op,e)                    -> mk_con "UnaryAssign" [pp_unassop op ; string_of_expr e]
      | Array       {name = e1 ; size = e2}                   -> mk_con "Array" [string_of_expr e1; string_of_expr e2]
      | InlineIf {cond = e1; true_expr = e2; 
                  false_expr = e3}            -> mk_con "InlineIf" [string_of_expr e1; string_of_expr e2;
                                                                                       string_of_expr e3]
      | FuncCall {name=n; parameters=p}       -> mk_con "FuncCall" [n; string_of_expr_list p]
      | Delete e                              -> mk_con "Delete" [string_of_expr e]
      | New {ttype=t; size=e}                 -> mk_con "New" [pp_type t; "["; string_of_expr e;"]"]
      | TypeCast{new_type = t; 
                casted_expr = e}              -> mk_con "TypeCast" [pp_type t;string_of_expr e]
                                                       
  let string_of_jumpname = function
      | Break    -> "Break"
      | Continue -> "Continue"
  
  (*let rec string_of_stmt = function
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
          | Return (_,e)                                     -> make_con "Return" [string_of_expr e] *)
  
  