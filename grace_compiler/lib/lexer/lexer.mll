{
open Parser
open Lexing

exception LexicalError of string

let string_buff = Buffer.create 256

let increase_lnum lexbuf = 
  let 
    pos = lexbuf.Lexing.lex_curr_p 
  in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = lexbuf.lex_curr_pos; }

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | '0' -> '\000'
  | c   -> c

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
let backslash_escapes = ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']

rule lexer = parse 
  | "and"      { T_and        }
  | "char"     { T_char       }
  | "div"      { T_div        }
  | "do"       { T_do         }
  | "else"     { T_else       }
  | "fun"      { T_fun        }
  | "if"       { T_if         }
  | "int"      { T_int        }
  | "mod"      { T_mod        }
  | "not"      { T_not        }
  | "nothing"  { T_nothing    }
  | "or"       { T_or         }
  | "ref"      { T_ref        }
  | "return"   { T_return     }
  | "then"     { T_then       }
  | "var"      { T_var        }
  | "while"    { T_while      }
  | '+'        { T_plus       }     
  | '-'        { T_minus      }  
  | '*'        { T_times      }
  | '='        { T_eq         }  
  | '#'        { T_neq        }  
  | "<-"       { T_assign     }  
  | '>'        { T_gt         }      
  | '<'        { T_lt         }       
  | ">="       { T_ge         }       
  | "<="       { T_le         }  
  | ';'        { T_semicol    }           
  | '('        { T_lparen     }          
  | ')'        { T_rparen     }          
  | '['        { T_lbracket   }            
  | ']'        { T_rbracket   }            
  | '{'        { T_lcurl      }         
  | '}'        { T_rcurl      }   
  | ':'        { T_colon      }         
  | ','        { T_comma      } 
        
  |  digit+ { T_intconst (int_of_string (lexeme lexbuf))} 
  | (letter)(letter|digit|'_')* {T_id (lexeme lexbuf)}
  | '\'' [^ '\\' '\'' '\"'] '\'' { T_charconst (lexeme_char lexbuf 1) }
  | '\'' esc_char '\'' { T_charconst (char_for_backslash @@ lexeme_char lexbuf 2)  }
  | '\'' '\\' (_ as c)
      { raise_lex_err_exception lexbuf
          (Printf.sprintf "Illegal escape sequence \\%c" c)
      }
  | '\'' const_char '\'' { T_charconst (lexeme lexbuf).[1]}
  (* | '\"' const_char* '\"' { T_stringliteral (lexeme lexbuf)}  *)
  | '"' { Buffer.clear string_buff;
          string lexbuf;
          T_stringliteral (Buffer.contents string_buff) }
  | whitespace+ {lexer lexbuf} (* consume whitespaces *)  
  | newline {increase_lnum lexbuf; lexer lexbuf} (*consume newlines *)
  | eof {T_eof}


  | "$$" { (* enter multiline comment *) multiline_comment lexbuf }
  | '$'  { (* enter single line comment *) line_comment lexbuf }
  | _ as chr     { raise_lex_err_exception lexbuf (Printf.sprintf "unmatched char: '%c' (ascii: %d)"
                    chr (Char.code chr));
                    lexer lexbuf }
  
and multiline_comment = parse     
  | "$$" { lexer lexbuf } (* somehow nested comments are not allowed because the closest "*/" is matched but idk why *)
  | eof  { raise_lex_err_exception lexbuf "Multi-line comments cannot span in multiple files"; (* "Multi-line comments cannot span in multiple files" *)
            lexer lexbuf }
  | newline {increase_lnum lexbuf; multiline_comment lexbuf}
  | _ { (* nothing *) multiline_comment lexbuf }

  and line_comment = parse 
    eof  { (* exit comment *) lexer lexbuf }
  | newline { increase_lnum lexbuf; lexer lexbuf}
  | _    { (* nothing *) line_comment lexbuf } 

and string = parse
  | '"'
      { () }
  | '\\' (backslash_escapes as c)
      { Buffer.add_char string_buff (char_for_backslash c);
        string lexbuf }
  | _ as c
      { Buffer.add_char string_buff c;
        string lexbuf }

{

}
