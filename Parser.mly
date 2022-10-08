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
%token T_intconst 
%token T_doubleconst
%token T_id
%token T_charconst
%token T_stringliteral
%token T_special_char

%nonassoc "==" "!=" ">" "<" "<=" ">="
%left "*" "/" "%" "+" "-" "&&" "||"
%right "=" "+=" "−=" "*=" "/=" "%="

%start program
%type<unit>program
%type<unit>declaration


%%

program : declaration_list {}
         | T_eof {}
;

declaration_list: declaration_list declaration {}
; 
declaration : variable_declaration {}
            | function_declaration {}
            | function_definition {}
;

variable_declaration : type declarator "," variable_declaration ";"
;


type : basic_type ["*"]
;
basic_type : "int" 
            | "char"
            | "bool"
            | "double"
;
declarator : T_id ["[" constant-expression "]"]
;

function_declaration: result_type T_id "(" parameter_list? ")"
;

result_type: type 
            | "void"
;

parameter_list: parameter ("," parameter)*
;
parameter: ["byref"] type T_id
;
function_definition: result_type T_id "(" [parameter_list]  ")" ";"
                     "{" (declaration)* (statement)* "}"
;
statement: ";"
          |expression ";"
          |"{" (statement)* "}"
          |"if" "(" expression ")" statement ["else" statement]
          |[T_id ":" ] "for" "(" [expression] ";" [expression] ";" [expression] ")" statement
          |"continue" [T_id] ";"
          |"break" [T_id] ";"
          |"return" [expression] ";"
expression:
          T_id
          |"(" expression ")"  
          |"true"
          | "false"
          | "NULL"
          | T_intconst
          | T_charconst
          | T_doubleconst
          | T_stringliteral
          | T_id "(" [expression-list] ")"
          | expression "[" expression "]"
          | unary-operator expression
          | expression binary-assignment expression
          | "(" type ")" expression
          | expression "?" expression ":" expression
          | "new" type [ "[" expression "]" ]
          | "delete" expression
;

expression_list: expression ("," expression)*
;
constant_expression: expression
;
unary_operator:  "&"
               | "*"
               | "+"
               | "-"
               | "!"
;
binary_operator:  "*"
                | "/"
                | "%"
                | "+"
                | "-"
                | "<"
                | ">"
                | "<="
                | ">="
                | "=="
                | "!="
                | "&&"
                | "||"
                | ","
;

unary_assignment:  "++"
                 | "--"
;

binary_assignment:  "="
                  | "*="
                  | "/="
                  | "%="
                  | "+="
                  | "-="
