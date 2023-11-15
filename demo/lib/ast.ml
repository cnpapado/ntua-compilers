module type AST = sig 
  val f : int -> string
end

module AST_parse_info : AST = struct
  let f x = "hi" ^ string_of_int x
end

