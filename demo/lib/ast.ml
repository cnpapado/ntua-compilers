(** Encapsulates the type of an ast node *)
module type Node = sig type t end


(** The grammar of all ASTs, parameterized by the node type *)
module MakeAST (Node : Node) = struct
  type expr = Bool of {b:bool; meta: Node.t} | Int of {i:int; meta: Node.t}
  (* ... *)
end


(** A basic AST *)
module BasicAST = struct 
  type basic_node = int 
  include MakeAST (struct type t = basic_node end)
end


(* An AST with location info *)
module ParserAST = struct 
  type node_with_loc_info = {fname: string; line_no: int; char_no: int} 
  include MakeAST (struct type t = node_with_loc_info end)
end


(* An AST with type info*)
module TypedAST = struct 
  type node_with_type_info = {typ: int} 
  include MakeAST (struct type t = node_with_type_info end)
end