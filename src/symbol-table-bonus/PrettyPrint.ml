open Types
open Ast

exception ConstantEvaluationError

type loc = Lexing.position 

let string_of_loc loc =
  Printf.sprintf "Line:%d Position:%d" loc.Lexing.pos_lnum
    (loc.Lexing.pos_cnum - loc.Lexing.pos_bol + 1)

let rec string_of_type t = 
    match t with
    | TYPE_none -> "This should never be printed!"        
    | TYPE_int  -> "int"
    | TYPE_double -> "double"
    | TYPE_char   -> "char"
    | TYPE_bool   -> "bool"
    | TYPE_array {ttype = typ; size = _} -> Printf.sprintf "%s array" (string_of_type typ)
    | TYPE_ptr {ttype = typ; level=l} -> Printf.sprintf ("%s pointer of level %d") (string_of_type typ) l
    | TYPE_void   -> "void"  

let pp_uop op =
    match op with  
    | BitAnd         -> "&"
    | UTimes         -> "*"
    | UPlus          -> "+"
    | UMinus         -> "-"
    | BitNot         -> "!"

let pp_uass op = 
    match op with
    | PrePlusPlus
    | PostPlusPlus   -> "++"
    | PreMinusMinus
    | PostMinusMinus -> "--"


(*function to print binary operators in string format*)
let pp_bop op =
    match op with 
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

let pp_bass op = 
    match op with
    | Assign  -> "="
    | TimesEq -> "*="
    | DivEq   -> "/="
    | ModEq   -> "%="
    | PlusEq  -> "+="
    | MinusEq -> "-="