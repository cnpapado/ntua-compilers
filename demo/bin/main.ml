open Demo.Ast
(* let () = print_endline (Ast.AST.f (Ast.AST.B(true))) *)
let get_str_parserAST e = 
  match e with 
  | ParserAST.Int({node=z; meta=_}) -> string_of_int z
  | ParserAST.Bool({node=z; meta=_}) -> if z then "true" else "false"

let get_str_basicAST e = 
  match e with 
  | BasicAST.Int(z) -> string_of_int z
  | BasicAST.Bool(z) -> if z then "true" else "false"

let () = 
  print_endline 
    ( (get_str_parserAST (ParserAST.Int({node=1; meta=234354;}))) 
      ^ "\n" 
      ^ (get_str_basicAST (BasicAST.Bool(true)))
    )
