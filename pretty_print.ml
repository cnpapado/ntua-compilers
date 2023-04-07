let string_of_loc loc = 
  "line " ^ (string_of_int (loc.Lexing.pos_lnum)) ^ ", " ^ 
  "position " ^ (string_of_int ((loc.Lexing.pos_cnum - loc.Lexing.pos_bol) + 1))

open Format

(*function to print type in string format*)
let rec pp_type = function 
    | TYPE_int  -> "int" 
    | TYPE_char -> "char" 
    | TYPE_bool -> "bool"
    | TYPE_double -> "double" 

(*function to print unary operators in string format*)
let pp_uop = function 
    | T_bitand     -> "&"
    | T_times      -> "*"
    | T_plus       -> "+"
    | T_minus      -> "-"
    | T_bitnot     -> "!"
    | T_plusplus   -> "++"
    | T_minusminus -> "--"

(*function to print binary operators in string format*)
let pp_bop = function
    | T_times   -> "*"
    | T_div     -> "/"
    | T_mod     -> "%"
    | T_plus    -> "+"
    | T_minus   -> "-"
    | T_lt      -> "<"
    | T_gt      -> ">"
    | T_le      -> "<="
    | T_ge      -> ">="
    | T_eq      -> "=="
    | T_neq     -> "!="
    | T_and     -> "&&"
    | T_or      -> "||"
    | T_comma   -> ","
    | T_assign  -> "="
    | T_timeseq -> "*="
    | T_diveq   -> "/="
    | T_modeq   -> "%="
    | T_pluseq  -> "+="
    | T_minuseq -> "-="



let mk_con con l = 
    let rec aux carry = function 
      | [] -> carry ^ ")"
      | [s] -> carry ^ s ^ ")"
      | s::rest -> aux (carry ^ s ^ ", ") rest 
    in aux (con ^ "(") l 

let rec string_of_expr = function 
    | Unit _                                  -> "Unit"
    | Ident       (_, s)                      -> make_con "Ident" s
    | Bool        (_, b)                      -> make_con "Bool" (string_of_bool b)
    | Int         (_, x)                      -> make_con "Int" (string_of_int x)
    | Char        (_, x)                      -> make_con "Char" x
    | Float       (_, x)                      -> make_con "Float" (string_of_float x)
    | BinExpr     (_,op,e1,e2)                -> make_con "BinExpr" [string_of_expr e1 pp_bop op string_of_expr e2]
    | BinAssign   (_,op,e1,e2)                -> make_con "BinAssign" [string_of_expr e1 pp_bop op string_of_expr e2]
    | UnaryExpr   (_,op,e)                    -> make_con "UnaryExpr" [pp_bop op string_of_expr e]
    | UnaryAssign (_,op,e)                    -> make_con "UnaryAssign" [pp_bop op string_of_expr e]
    | FuncCall    (_,{name:n;parameters: p})  -> make_con "FuncCallname" [mk_con "name" [n] ; 
                                                                               mk_con "parameters" p
                                                                              ]
    | Array (_,e1,e2)                         -> make_con "Array" [e1; mk_con "" [e2]]
    | InlineIf (_,e1,e2,e3)                   -> make_con "InlineIf" [string_of_expr e1; string_of_expr e2;
                                                                      string_of_expr e3]