let string_of_loc loc = 
  "line " ^ (string_of_int (loc.Lexing.pos_lnum)) ^ ", " ^ 
  "position " ^ (string_of_int ((loc.Lexing.pos_cnum - loc.Lexing.pos_bol) + 1))
open Types
open Ast
open Format

type unary_op = 
              | T_bitand    
              | T_times     
              | T_plus      
              | T_minus     
              | T_bitnot    
              | T_plusplus  
              | T_minusminus

type binary_op = | T_times  
                 | T_div    
                 | T_mod    
                 | T_plus   
                 | T_minus  
                 | T_lt     
                 | T_gt     
                 | T_le     
                 | T_ge     
                 | T_eq     
                 | T_neq    
                 | T_and    
                 | T_or     
                 | T_comma  
                 | T_assign 
                 | T_timeseq
                 | T_diveq  
                 | T_modeq  
                 | T_pluseq 
                 | T_minuseq

(*function to print type in string format*)
let rec pp_type = function 
    | TYPE_int  -> "int" 
    | TYPE_char -> "char" 
    | TYPE_bool -> "bool"
    | TYPE_double -> "double" 
    | TYPE_array {ttype = t; size = sz} -> "Array"

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

let rec string_of_expr e:expr = match e with 
    | Unit                                  -> "Unit"
    | Ident       s                         -> make_con "Ident" s
    | Bool        b                         -> make_con "Bool" (string_of_bool b)
    | Int         x                         -> make_con "Int" (string_of_int x)
    | Char        x                         -> make_con "Char" x
    | Float       x                         -> make_con "Float" (string_of_float x)
    | BinExpr     (op,e1,e2)                -> make_con "BinExpr" [string_of_expr e1 pp_bop op string_of_expr e2]
    | BinAssign   (op,e1,e2)                -> make_con "BinAssign" [string_of_expr e1 pp_bop op string_of_expr e2]
    | UnaryExpr   (op,e)                    -> make_con "UnaryExpr" [pp_bop op string_of_expr e]
    | UnaryAssign (op,e)                    -> make_con "UnaryAssign" [pp_bop op string_of_expr e]
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
 