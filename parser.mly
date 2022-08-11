(*yyparse: called once, includes semantics, basically feeds the whole frontend process*)
%{

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


%%

program : declaration+ T_eof
;

declaration : variable-declaration
            | function-declaration
            | function-definition
;

variable-declaration : type declarator ("," declarator)* ";"
;

type : basic-type ("*")*
;
basic-type : "int" 
            | "char"
            | "bool"
            | "double"
;
declarator : T_charconst ("[" constant-expression "]")?
             T_stringconst ("[" constant-expression "]")?
;

function-declaration: result-type T_charconst "(" parameter_list? ")"
                    | result-type T_stringconst "(" parameter_list? ")"

;

result-type: type 
            | "void"
;

parameter_list: parameter ("," parameter)*
;
parameter: ("byref")? type T_charconst
          |("byref")? type T_stringconst
;
function-definition: T_charconst ("[" parameter_list "]")+ 
                     "{" (declaration)* (statement)* "}"
;
statement: ";"
          |"(" expression ")"
          |"{" (statement)* "}"
          |"if" "(" expression ")" statement ("else" statement)*
          |(T_charconst ":" )? "for" "(" (expression)? ";" (expression)? ";" (expression)? ")" statement
          |(T_stringliteral ":" )? "for" "(" (expression)? ";" (expression)? ";" (expression)? ")" statement
          |"continue" (T_charconst)? ";"
          |"continue" (T_stringliteral)? ";"
          |"break" (T_charconst)? ";"
          |"break" (T_stringliteral)? ";"
          |"return" (expression)? ";"
expression: 
          |"true"
          | "false"
          | "NULL"
          | T_intconst
          | T_charconst
          | T_doubleconst
          | T_stringliteral
          |
