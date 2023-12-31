open Ast
open Symbol
open Types

let string_of_char = String.make 1

let str_of_header = function 
  | SemAST.Header x -> 
    let str_of_param (pm, id, t) = 
      begin match pm with PASS_BY_REFERENCE -> "byref " | PASS_BY_VALUE -> "" end ^ 
     (pp_typ @@ Some t) ^ id
    in
    "fun " ^ x.header_id ^ 
    "(" ^ String.concat "," (List.map str_of_param x.header_fpar_defs) ^ ") ->" ^ (pp_typ @@ Some x.header_ret)

let str_of_vardef v = match v with 
  | SemAST.VarDef(x) ->
  "var " ^ (String.concat "," x.var_def_id) ^ ":" ^ (pp_typ @@ Some x.var_def_typ)

let rec str_of_expr = function
  | SemAST.Int x -> "Int(" ^ (string_of_int x.i) ^ ")"
  | SemAST.Char x -> "Int(" ^ (string_of_char x.c) ^ ")"
  | SemAST.Lvalue l -> "Lval(" ^ (str_of_lval l) ^ ")"
  | SemAST.ExprFuncCall f -> "ExprFuncCall(" ^ (str_of_funccall f) ^ ")"
  | SemAST.SignedExpr {sign; e; meta=_} -> Printf.sprintf "SignedExpr(%s, %s)" (str_of_uop sign) (str_of_expr e)
  | SemAST.BinExpr {l; r; op; meta=_} -> "BinExpr(" ^ (str_of_expr l) ^ (str_of_aritm op) ^ (str_of_expr r) ^ ")"


and str_of_lval = function
  | SemAST.LvalueId {id; meta=_} -> id 
  | SemAST.LvalueString {s; meta=_} -> "str(" ^ s ^ ")" 
  | SemAST.LvalueArr {arr=(l,e); meta=_} -> (str_of_lval l) ^ "[" ^ (str_of_expr e) ^ "]"

and str_of_aritm = function 
  | SemAST.Times -> "*"
  | SemAST.Div -> "\\"
  | SemAST.Mod -> "%"
  | SemAST.Plus -> "+"
  | SemAST.Minus -> "-"

and str_of_uop = function
  | SemAST.UPlus -> "+" 
  | SemAST.UMinus -> "-"

and str_of_funccall = function 
  | SemAST.FuncCall {name; parameters; meta=_} ->
    name ^ "(" ^ String.concat "," (List.map str_of_expr parameters) ^ ")"

let rec str_of_cond = function
  | SemAST.ExprCond {l; r; op; meta=_} -> "(" ^ (str_of_expr l) ^ (str_of_comp op) ^ (str_of_expr r) ^ ")"
  | SemAST.CompoundCond {l; r; op; meta=_} -> "(" ^ (str_of_cond l) ^ (str_of_log op) ^ (str_of_cond r) ^ ")"
  | SemAST.NegatedCond c -> "not" ^ (str_of_cond c)
  
and str_of_comp = function 
  | Lt -> "<"
  | Gt -> ">"
  | Le -> "<="
  | Ge -> ">="
  | Eq -> "=="
  | Neq -> "!=" 
and str_of_log = function 
  | And -> "&&" | Or -> "||"
and str_of_stmt = function 
  | SemAST.EmptyStmt -> "EmptyStmt"
  | SemAST.Assign {lvalue; rvalue; meta=_} -> Printf.sprintf "Assign: %s = %s" (str_of_lval lvalue) (str_of_expr rvalue)
  | SemAST.Block stmt_list -> String.concat "\n" (List.map str_of_stmt stmt_list)
  | SemAST.StmtFuncCall f -> "StmtFuncCall(" ^ (str_of_funccall f) ^ ")"
  | SemAST.If i -> "if" ^ (str_of_cond i.if_cond) ^ "{" ^ (str_of_stmt i.ifstmt) ^ "}" ^ 
    begin match i.elsestmt with None -> "" | Some s -> (str_of_stmt s) end
  | SemAST.While w -> "while" ^ (str_of_cond w.while_cond) ^ "{" ^ (str_of_stmt w.whilestmt) ^ "}"
  | SemAST.Return r -> "return" ^ match r.ret with None -> "" | Some e -> (str_of_expr e)
  
let rec str_of_localdef x = match x with 
  | SemAST.FuncDef func_def -> "def:" ^ str_of_funcdef func_def
  | SemAST.FuncDecl header -> "decl:" ^ str_of_header header ^ ";"
  | SemAST.VarDef _ -> str_of_vardef x

and str_of_funcdef x = 
  (str_of_header x.func_def_header) ^ "{\n" ^
  (String.concat "\n" (List.map str_of_localdef x.func_def_local)) ^ "\n" ^
  (str_of_stmt x.func_def_block) ^ "}"
                
let str_of_ast = str_of_localdef (* function SemAST.FuncDef func_def -> str_of_funcdef func_def *)