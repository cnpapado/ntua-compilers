(** Encapsulates the type of an ast node *)
module type Node = sig type 'a t end


(** The grammar of all ASTs, parameterized by the node type *)
module MakeAST (Node : Node) = struct
  type expr = Bool of bool Node.t | Int of int Node.t
  (* ... *)
end


(** A basic AST *)
module BasicAST = struct 
  type 'a basic_node = 'a 
  include MakeAST (struct type 'a t = 'a basic_node end)
end


(** An AST with location info *)
module ParserAST = struct 
  type 'a node_with_loc_info = {node: 'a; meta: int} 
  include MakeAST (struct type 'a t = 'a node_with_loc_info end)
end


(** An AST with type info*)
module TypedAST = struct 
  type 'a node_with_type_info = {node: 'a; typ: int} 
  include MakeAST (struct type 'a t = 'a node_with_type_info end)
end