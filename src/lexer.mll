{
open Parser
open Lexing

exception LexicalError of string

let increase_lnum lexbuf = 
  let 
    pos = lexbuf.Lexing.lex_curr_p 
  in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = lexbuf.lex_curr_pos; }


let [@warning "-21"] raise_lex_err_exception lexbuf msg = (* line numbering is not working *)
  let p = lexbuf.Lexing.lex_start_p in
  let line = p.Lexing.pos_lnum in
  let col = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  raise (LexicalError msg)
}


let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let whitespace = [' ' '\t' '\r']
let newline = ('\010' | "\013\010" )
let ascii = ['0'-'9' 'A'-'F' 'a'-'f']          
let ascii_escape = ('\\' 'x' ascii ascii)
let esc_char = "\\t" | "\\r" | "\\n" | "\\'" | "\\\\" | "\\\"" | "\\0" | (ascii_escape) (* all escape chars including whitespaces *)
let const_char = (_ # ['\'' '\"' '\\'] | esc_char)?  
(*when '\'' the input ''' gets rejected-rightfully so- while the escape chars ensure that '\'' gets accepted *) 
(* ^            ['\t' '\r' '\n' '\'' '\"' '\\']|  *)

rule lexer = parse 
    "bool"     { T_bool }
  | "break"    { T_break      } 
  | "byref"    { T_byref      }
  | "char"     { T_char       }
  | "continue" { T_continue   }
  | "delete"   { T_delete     }
  | "double"   { T_double     }
  | "else"     { T_else       }
  | "for"      { T_for        }
  | "false"    { T_false      }
  | "if"       { T_if         }
  | "int"      { T_int        }
  | "new"      { T_new        }
  | "NULL"     { T_NULL       }
  | "return"   { T_return     }
  | "true"     { T_true       }
  | "void"     { T_void       }
  | '='        { T_assign     }              
  | "=="       { T_eq         }       
  | "!="       { T_neq        }        
  | '>'        { T_gt         }      
  | '<'        { T_lt         }       
  | ">="       { T_ge         }       
  | "<="       { T_le         }       
  | '+'        { T_plus       }     
  | '-'        { T_minus      }      
  | '*'        { T_times      }      
  | '/'        { T_div        }    
  | '%'        { T_mod        }       
  | '&'        { T_bitand     }          
  | '!'        { T_bitnot     }          
  | "&&"       { T_and        }        
  | "||"       { T_or         }       
  | '?'        { T_q          }     
  | ':'        { T_colon      }         
  | ','        { T_comma      }         
  | "++"       { T_plusplus   }             
  | "--"       { T_minusminus }               
  | "+="       { T_pluseq     }           
  | "-="       { T_minuseq    }            
  | "*="       { T_timeseq    }          
  | "/="       { T_diveq      }       
  | "%="       { T_modeq      }       
  | ';'        { T_semicol    }           
  | '('        { T_lparen     }          
  | ')'        { T_rparen     }          
  | '['        { T_lbracket   }            
  | ']'        { T_rbracket   }            
  | '{'        { T_lcurl      }         
  | '}'        { T_rcurl      }         
  |  digit+ { T_intconst (int_of_string (lexeme lexbuf))} 
  |  digit+ '.' digit+ ( ['e' 'E'] ['+' '-']? digit+ )? { T_doubleconst (float_of_string (lexeme lexbuf))}
  | (letter)(letter|digit|'_')* {T_id (lexeme lexbuf)}
  
  | '\'' const_char '\'' { T_charconst (lexeme lexbuf).[0]}
  | '\"' const_char* '\"' { T_stringliteral (lexeme lexbuf)} (* ??????????? is stringconst the parser's strinliteral*)
  | whitespace+ {lexer lexbuf} (* consume whitespaces *)  
  | newline {increase_lnum lexbuf; lexer lexbuf} (*consume newlines *)
  | eof {T_eof}


  | "/*" { (* enter multiline comment *) multiline_comment lexbuf }
  | "//" { (* enter single line comment *) line_comment lexbuf }
  | ('#' whitespace* "include" whitespace* '\"' const_char* '\"') {lexer lexbuf}
  | _ as chr     { raise_lex_err_exception lexbuf (Printf.sprintf "unmatched char: '%c' (ascii: %d)"
                    chr (Char.code chr));
                    lexer lexbuf }
  
  and multiline_comment = parse     (* why this matches to the closest "*/" ?? *)
    "/*" { multiline_comment lexbuf }
  | "*/" { lexer lexbuf } (* somehow nested comments are not allowed because the closest "*/" is matched but idk why *)
  | eof  { raise_lex_err_exception lexbuf "Multi-line comments cannot span in multiple files"; (* "Multi-line comments cannot span in multiple files" *)
            lexer lexbuf }
  | newline {increase_lnum lexbuf; multiline_comment lexbuf}
  | _ { (* nothing *) multiline_comment lexbuf }

  and line_comment = parse 
    eof  { (* exit comment *) lexer lexbuf }
  | newline { increase_lnum lexbuf; lexer lexbuf}
  | _    { (* nothing *) line_comment lexbuf } 



{
  (* let string_of_token token =
    match token with
      | T_bool     -> "T_bool"
      | T_break    -> "T_break"
      | T_byref    -> "T_byref"
      | T_char     -> "T_char"
      | T_continue -> "T_continue"
      | T_delete   -> "T_delete"
      | T_double   -> "T_double"
      | T_else     -> "T_else"
      | T_for    -> "T_for"
      | T_false     -> "T_false"
      | T_if   -> "T_if"
      | T_new     -> "T_new"
      | T_NULL -> "T_NULL"
      | T_return -> "T_return"
      | T_true   -> "T_true"
      | T_void  -> "T_void"
      | T_intconst  -> "T_intconst"
      | T_doubleconst  -> "T_doubleconst"
      | T_id  -> "T_id"
      | T_charconst  -> "T_charconst"
      | T_stringliteral  -> "T_stringliteral"
      | T_int -> "T_int"
      | T_eof -> "EOF"
      | T_special_char -> "special symbol"  *)


(* let main =
    let lexbuf = Lexing.from_channel stdin in
    let rec loop () =
      let token = lexer lexbuf in
      Printf.printf "token=%s, lexeme=\"%s\"\n"
        (string_of_token token) (Lexing.lexeme lexbuf);
      if token <> T_eof then loop () in
    loop ()
*)
}
