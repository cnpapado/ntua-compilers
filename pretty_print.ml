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

let fstring ppf s = fprintf ppf "%s" s
let pp_type ppf t = fstring ppf (pp_type t) 
let pp_unary ppf op = fstring ppf (pp_uop op) 
let pp_binary ppf op = fstring ppf (pp_bop op)

let rec pp_expr ppf = function 
    | Unit _                    -> fstring ppf "()"
    | Ident       (_, s)        -> fstring ppf s
    | Bool        (_, b)        -> fstring ppf (string_of_bool b)
    | Int         (_, x)        -> fstring ppf (string_of_int x)
    | Char        (_, x)        -> fstring ppf x
    | Float       (_, x)        -> fstring ppf (string_of_float x)
    | BinExpr     (_,op,e1,e2)  -> fprintf ppf "(%a %a %a)" pp_binary op pp_expr e1 pp_expr e2 
    | BinAssign   (_,op,e1,e2)  -> fprintf ppf "(%a %a %a)" pp_binary op pp_expr e1 pp_expr e2
    | UnaryExpr   (_,op,e)      -> fprintf ppf "%a(%a)" pp_unary op pp_expr e
    | UnaryAssign (_,op,e)      -> fprintf ppf "%a(%a)" pp_unary op pp_expr e
    | FuncCall () (*Check again to see if it matches symbol table actions*)
    | Array of loc*expr*expr
    | InlineIf of loc*expr*expr*expr