open Lexer

let string_of_token token =
  match token with
    | T_and           -> "T_and"
    | T_char          -> "T_char"
    | T_div           -> "T_div"
    | T_do            -> "T_do"
    | T_else          -> "T_else"
    | T_fun           -> "T_fun"
    | T_if            -> "T_if"
    | T_int           -> "T_int"
    | T_mod           -> "T_mod"
    | T_not           -> "T_not"
    | T_nothing       -> "T_nothing"
    | T_or            -> "T_or"
    | T_ref           -> "T_ref"
    | T_return        -> "T_return"
    | T_then          -> "T_then"
    | T_var           -> "T_var"
    | T_while         -> "T_while"
    | T_plus          -> "T_plus"     
    | T_minus         -> "T_minus"  
    | T_times         -> "T_times"
    | T_eq            -> "T_eq"  
    | T_hash          -> "T_hash"  
    | T_assign        -> "T_assign"  
    | T_gt            -> "T_gt"      
    | T_lt            -> "T_lt"       
    | T_ge            -> "T_ge"       
    | T_le            -> "T_le"  
    | T_semicol       -> "T_semicol"           
    | T_lparen        -> "T_lparen"          
    | T_rparen        -> "T_rparen"          
    | T_lbracket      -> "T_lbracket"            
    | T_rbracket      -> "T_rbracket"            
    | T_lcurl         -> "T_lcurl"         
    | T_rcurl         -> "T_rcurl"   
    | T_colon         -> "T_colon"         
    | T_comma         -> "T_comma" 
    | T_id(_)            -> "T_id"
    | T_charconst(_)     -> "T_charconst"
    | T_stringliteral(_) -> "T_stringliteral"
    | T_intconst(_)      -> "T_intconst"
    | T_eof -> "T_eof"


let main =
  let lexbuf = Lexing.from_channel stdin in
  let rec loop () =
    let token = lexer lexbuf in
    Printf.printf "token=%s, lexeme=\"%s\"\n"
      (string_of_token token) (Lexing.lexeme lexbuf);
    if token <> T_eof then loop () in
  loop ()