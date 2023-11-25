open Demo.Ast
(* let () = print_endline (Ast.AST.f (Ast.AST.B(true))) *)
 let get_str_parserAST e = 
  match e with 
  | ParserAST.Int({i=z; meta=m}) -> string_of_int z ^ " at " ^ m.fname ^ ":" ^ string_of_int m.line_no ^ ":" ^ string_of_int m.char_no 
  | ParserAST.Bool({b=z; meta=m}) -> (if z then "true" else "false") ^ " at " ^ m.fname ^ ":" ^ string_of_int m.line_no ^ ":" ^ string_of_int m.char_no

let get_str_basicAST e = 
  match e with 
  | BasicAST.Int({i=z; meta=_}) -> string_of_int z
  | BasicAST.Bool({b=z; meta=_}) -> if z then "true" else "false"

let () = 
  print_endline 
    ( (get_str_parserAST (ParserAST.Int({i=1; meta={fname="foo"; line_no=12; char_no=2};}))) 
      ^ "\n"
      ^ (get_str_basicAST (BasicAST.Bool({b=true; meta=42})))
    )
