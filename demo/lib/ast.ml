(* module type AST_types = sig 
  type expr = I of int | B of bool
  val f : expr -> string
end *)

(* module AST = struct
  type expr = I of int | B of bool
  let f x = 
    match x with 
    | I(y) -> "int " ^ string_of_int y
    | B(y) -> "bool " ^ if y then "true" else "false"
end


module ParseST = struct 
  include AST
  type 'a PST_t = {node: 'a; metadata: int} 
end *)


(* module AST : sig 
  include module type of AST_types
  val make_expr : int -> expr 
end = struct 
  type expr = I of int
  and binop = Plus of int*int | Minus of int*int

  let f x = match x with I(x) -> "hi" ^ string_of_int x
  let make_expr x = I(x) 
end *)


(* type metadata = int

module AST_parse_info : AST_types = struct

  type expr = {node: AST.expr; meta: metadata}
  and binop = {line_no: int; char_no: int}  

  let f x = "bye" ^ string_of_int x

end *)


type 'a parse_info_node = {node: 'a; meta: int} 
module P_info_AST = struct 
  type expr = Bool of bool parse_info_node | Int of int parse_info_node
end

type 'a ast_node = 'a
module AST = struct 
  type expr = Bool of bool ast_node | Int of int ast_node
end


(* module A = struct 
  type 'a node
  type expr = Bool of bool node
end

module B = struct
  type node = bool
end *)