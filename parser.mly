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

//Special chars
%token T_assign     // "="
%token T_eq         // "=="
%token T_neq        // "!="
%token T_gt         // ">"
%token T_lt         // "<" 
%token T_ge         // ">="
%token T_le         // "<="
%token T_plus       // "+"
%token T_minus      // "-"
%token T_times      // "*"
%token T_div        // "/"
%token T_mod        // "%"
%token T_bitand     // "&"
%token T_bitnot     // "!"
%token T_and        // "&&"
%token T_or         // "||"
%token T_q          // "?"
%token T_colon      // ":"
%token T_comma      // ","
%token T_plusplus   // "++"
%token T_minusminus // "--"
%token T_pluseq     // "+="
%token T_minuseq    // "-="
%token T_timeseq    // "*="
%token T_diveq      // "/="
%token T_modeq      // "%="
%token T_semicol    // ";"
%token T_lparen     // "("
%token T_rparen     // ")"
%token T_lbracket   // "["
%token T_rbracket   // "]"
%token T_lcurl      // "{"
%token T_rcurl      // "}"

%nonassoc T_eq T_neq T_gt T_lt T_le T_ge
%left T_times T_div T_mod T_plus T_minus T_and T_or
%right T_assign T_pluseq T_minuseq T_timeseq T_diveq T_modeq

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
            | function_declaration function_body { () }

            
;
function_body: /*nothing*/ { () } 
             | T_lbracket inside_brackets T_rbracket { () }
;

inside_brackets: optional_declaration_list optional_statement_list { () } /* check this ??????? */ 
;

declarator : T_id  { () } 
           | T_id T_lbracket constant_expression T_rbracket { () }
;

declarator_list : declarator { () }
                | declarator T_comma declarator_list { () }
;

variable_declaration : ttype declarator_list T_semicol { () }
;


ttype : basic_type optional_T_times{ () }
;

optional_T_times: /*nothing*/{ () }
                 | optional_T_times T_times { () }
;

basic_type : T_int  { () }
           | T_char { () }
           | T_bool { () }
           | T_double { () }
;

function_declaration : result_type T_id T_lparen parameter_list T_rparen { () }
                     | result_type T_id T_lparen T_rparen { () }
;

result_type : ttype { () }
            | T_void { () }
;


parameter_list : parameter { () }
               | parameter_list T_comma parameter { () }
;

parameter : ttype T_id { () } 
          | T_byref ttype T_id { () }
;


optional_expression : /* nothing */ { () }
                    | expression { () }
;

expression_list : expression { () }
                | expression T_comma expression_list { () }
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

statement : T_semicol { () }
          | expression T_semicol { () }
          | T_lcurl optional_statement_list T_rcurl { () }
          | T_if T_lparen expression T_rparen statement { () }
          | T_if T_lparen expression T_rparen statement T_else statement { () }
          | optional_T_id T_colon T_for T_lparen optional_expression T_semicol optional_expression T_semicol optional_expression T_rparen statement { () } 
          | T_continue optional_T_id T_semicol { () }
          | T_break optional_T_id T_semicol { () }
          | T_return optional_expression T_semicol { () }        
;

expression : T_id { () }
           | T_lparen expression T_rparen { () } 
           | T_true { () }
           | T_false { () }
           | T_NULL { () }
           | T_intconst { () }
           | T_charconst { () }
           | T_doubleconst { () }
           | T_stringliteral { () }
           | T_id T_lparen optional_expression_list T_rparen { () }
           | expression after_expression { () }
           | unary_operator expression { () }
           | unary_assignment expression { () }
           | T_lparen ttype T_rparen expression { () }
           | T_new ttype optional_new{ () }
           | T_delete expression { () }
;

after_expression: T_lbracket expression T_rbracket { () }
                | binary_operator expression { () }
                | unary_assignment { () }
                | binary_assignment expression { () }
                | T_q expression T_colon expression { () }

optional_new : /*nothing*/ { () }
             | T_lbracket expression T_rbracket { () }
constant_expression : expression { () } 
;

unary_operator : T_bitand { () }
               | T_times { () }
               | T_plus { () }
               | T_minus { () }
               | T_bitnot { () }
;

binary_operator : T_times { () }
                | T_div { () }
                | T_mod { () }
                | T_plus { () }
                | T_minus { () }
                | T_lt { () }
                | T_gt { () }
                | T_le { () }
                | T_ge { () }
                | T_eq { () }
                | T_neq { () }
                | T_and { () }
                | T_or { () }
                | T_comma { () }
;

unary_assignment : T_plusplus { () }
                 | T_minusminus { () }
;

binary_assignment : T_assign { () }
                  | T_timeseq { () }
                  | T_diveq { () }
                  | T_modeq { () }
                  | T_pluseq { () }
                  | T_minuseq { () }
;
