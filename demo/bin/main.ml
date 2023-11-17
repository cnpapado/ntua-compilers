open Demo.Ast
(* let () = print_endline (Ast.AST.f (Ast.AST.B(true))) *)
let get_str1 e = 
  match e with 
  | P_info_AST.Int({node=z; meta=_}) -> string_of_int z
  | P_info_AST.Bool({node=z; meta=_}) -> if z then "true" else "false"

let get_str2 e = 
  match e with 
  | AST.Int(z) -> string_of_int z
  | AST.Bool(z) -> if z then "true" else "false"

let () = 
  print_endline 
    ((get_str1 (P_info_AST.Int({node=1; meta=234354;}))) ^ "\n" ^ (get_str2 (AST.Bool(true))))
