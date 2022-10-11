/* yyparse: called once, includes semantics, basically feeds the whole frontend process */
%{
    open Printf
%}

%token T_bool 
%token T_break
%token T_byref
%token T_char 
%token T_continue 
%token T_delete 
%token T_double 
%token T_else 
%token T_for 
%token T_false 
%token T_if 
%token T_int 
%token T_new 
%token T_NULL 
%token T_return 
%token T_true 
%token T_void 
%token T_eof 
%token T_intconst 
%token T_doubleconst
%token T_id
%token T_charconst
%token T_stringliteral
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
basic_type : T_int  { () }
           | T_char { () }
           | T_bool { () }
           | T_double { () }
;

declarator : T_id { () } 
           | T_id "[" constant_expression "]" { () }
;


function_declaration : result_type T_id "(" parameter_list ")" { () }
                     | result_type T_id "(" ")" { () }
;

result_type : ttype { () }
            | T_void { () }
;


parameter_list : parameter { () }
               | parameter_list "," parameter { () }
;
parameter : ttype T_id { () } 
          | T_byref ttype T_id { () }
;

function_definition : result_type T_id "(" ")" ";"
                      "{" optional_declaration_list statement_list "}" { () } /* check this ??????? */
                    | result_type T_id "(" parameter_list ")" ";"
                      "{" optional_declaration_list statement_list "}" { () } /* check this ??????? */
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

statement_list : statement { () }
               | statement_list statement { () }
;

optional_statement_list : /* nothing */ { () }
                        | statement_list { () }
;

optional_T_id : /* nothing */ { () }
              | T_id { () }
;

statement : ";" { () }
          | expression ";" { () }
          | "{" optional_statement_list "}" { () }
          | T_if "(" expression ")" statement { () }
          | T_if "(" expression ")" statement "else" statement { () }
          | optional_T_id ":" T_for "(" optional_expression ";" optional_expression ";" optional_expression ")" statement { () } 
          | T_continue optional_T_id ";" { () }
          | T_break optional_T_id ";" { () }
          | T_return optional_expression ";" { () }        
;

expression :
           T_id { () }
           |"(" expression ")" { () } 
           | T_true { () }
           | T_false { () }
           | T_NULL { () }
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
           | T_new ttype { () }
           | T_new ttype "[" expression "]" { () }
           | T_delete expression { () }
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
