open Symbol
open Printf

exception ConstantEvaluationError

type loc = Lexing.position 

let string_of_loc loc =
  Printf.fprintf "Line:%d Position:%d" loc.Lexing.pos_lnum
    (loc.Lexing.pos_cnum - loc.Lexing.pos_bol + 1)

let rec string_of_type t = function
    | TYPE_none -> "This should never be printed!"        
    | TYPE_int  -> "int"
    | TYPE_double -> "double"
    | TYPE_char   -> "char"
    | TYPE_bool   -> "bool"
    | TYPE_array {ttype = typ; size = _} -> Printf.fprintf ("%s array") string_of_type typ
    | TYPE_ptr {ttype = typ; level=l} -> Printf.fprintf ("%s pointer of level %d") (string_of_type typ) l
    | TYPE_void   -> "void"  

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