/* yyparse: called once, includes semantics, basically feeds the whole frontend process */
%{
    open Printf
%}

%token T_bool "bool"
%token T_break "break"
%token T_byref "byref"
%token T_char "char"
%token T_continue "continue"
%token T_delete "delete"
%token T_double "double"
%token T_else "else"
%token T_for "for"
%token T_false "false"
%token T_if "if"
%token T_int "int"
%token T_new "new"
%token T_NULL "NULL"
%token T_return "return"
%token T_true "true"
%token T_void "void"
%token T_eof "eof"
%token<int> T_intconst 
%token<float> T_doubleconst
%token<string> T_id
%token<char> T_charconst
%token<string> T_stringliteral
%token T_special_char

%nonassoc "==" "!=" ">" "<" "<=" ">="
%left "*" "/" "%" "+" "-" "&&" "||"
%right "=" "+=" "-=" "*=" "/=" "%="

%start program
%type<unit> program
%type<unit> declaration


%%

/* Naming convension: E_list = E repeated 1 or more times
 *                    optional_E = E repeated 0 or 1 times
 *                    (thus optional_list_E = E repeated 0 or more times)
 */

program : optional_declaration_list T_eof { () }
;

optional_declaration_list : /* nothing */ { () }
                          | optional_declaration_list declaration { () }
; 

declaration : variable_declaration { () }
            | function_declaration { () }
            | function_definition { () }
;

declarator_list : declarator { () }
                | declarator "," declarator_list { () }

variable_declaration : ttype declarator_list ";" { () }
;


ttype : basic_type { () }
      | basic_type "*" { () }
;
basic_type : "int"  { () }
           | "char" { () }
           | "bool" { () }
           | "double" { () }
;

declarator : T_id { () } 
           | T_id "[" constant_expression "]" { () }
;


function_declaration : result_type T_id "(" parameter_list ")" { () }
                     | result_type T_id "(" ")" { () }
;

result_type : ttype { () }
            | "void" { () }
;


parameter_list : parameter { () }
               | parameter_list "," parameter { () }
;
parameter : ttype T_id { () } 
          | "byref" ttype T_id { () }
;

function_definition : result_type T_id "(" ")" ";"
                      "{" declaration_list statement_list "}" { () }
                    | result_type T_id "(" parameter_list ")" ";"
                      "{" declaration_list statement_list "}" { () } 
;

optional_expression : /* nothing */ { () }
                    | expression { () }
;

expression_list : expression { () }
                | expression "," expression_list { () }
;

optional_expression_list : /* nothing */ { () }
                         | expression_list { () }
;

optional_statement_list : /* nothing */ { () }
               | statement_list statement { () }
;

optional_T_id : /* nothing */ { () }
              | T_id { () }
;

statement : ";" { () }
          | expression ";" { () }
          | "{" optional_statement_list "}" { () }
          | "if" "(" expression ")" statement { () }
          | "if" "(" expression ")" statement "else" statement { () }
          | optional_T_id ":" "for" "(" optional_expression ";" optional_expression ";" optional_expression ")" statement { () } 
          | "continue" optional_T_id ";" { () }
          | "break" optional_T_id ";" { () }
          | "return" optional_expression ";" { () }        
;

expression :
           T_id { () }
           |"(" expression ")" { () } 
           |"true" { () }
           | "false" { () }
           | "NULL" { () }
           | T_intconst { () }
           | T_charconst { () }
           | T_doubleconst { () }
           | T_stringliteral { () }
           | T_id "(" optional_expression_list ")" { () }
           | expression "[" expression "]" { () }
           | unary_operator expression { () }
           | expression binary_assignment expression { () }
           | "(" ttype ")" expression { () }
           | expression "?" expression ":" expression { () }
           | "new" ttype { () }
           | "new" ttype "[" expression "]" { () }
           | "delete" expression { () }
;

constant_expression : expression { () } /* ????????????????????????? */
;

unary_operator : "&" { () }
               | "*" { () }
               | "+" { () }
               | "-" { () }
               | "!" { () }
;

binary_operator : "*" { () }
                | "/" { () }
                | "%" { () }
                | "+" { () }
                | "-" { () }
                | "<" { () }
                | ">" { () }
                | "<=" { () }
                | ">=" { () }
                | "==" { () }
                | "!=" { () }
                | "&&" { () }
                | "||" { () }
                | "," { () }
;

unary_assignment :  "++" { () }
                 | "--" { () }
;

binary_assignment :  "=" { () }
                  | "*=" { () }
                  | "/=" { () }
                  | "%=" { () }
                  | "+=" { () }
                  | "-=" { () }
;
