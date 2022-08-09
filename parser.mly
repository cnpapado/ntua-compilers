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
%token T_intconst 
%token T_realconst
%token T_id
%token T_charconst
%token T_stringconst
%token T_special_char

%nonassoc "==" "!=" ">" "<" "<=" ">="
%left "*" "/" "%" "+" "-" "&&" "||"
%right "=" "+=" "−=" "*=" "/=" "%="
"="
"=="
"!="
">"
"<"
">="
"<="
"+"
"-"
"*"
"/" 
"%"
"&"
"!"
"&&"
"||"
"?"
":"
","
"++"
"--"                       
"+="
"-="
"*="
"/="
"%="
";"
"("
")"
"["
"]"
"{"
"}"


%%