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


/* Precedence (proteraiothta) = the order in which (different) operations are performed 
   Associativity (prosetairistikothta) = operators of the same precedence in the absence of parentheses 

   %left/right declaration makes all operators left/right-associative 
   %nonassoc declares that it is a syntax error to find the same operator twice “in a row”
   
   The relative precedence of different operators is controlled by the order in which they are declared. The first %left
   or %right declaration in the file declares the operators whose precedence is lowest, the next such declaration
   declares the operators whose precedence is a little higher, and so on.
*/


// 
// %left T_plus T_minus 
// %left T_times T_div T_mod T_and T_or T_comma
// %right T_assign T_pluseq T_minuseq T_timeseq T_diveq T_modeq


%left T_comma
%right T_eq, T_pluseq, T_minuseq, T_timeseq, T_modeq, T_diveq
%left T_or 
%left T_and
%nonassoc T_assign T_neq T_gt T_lt T_le T_ge
%left T_plus, T_minus
%left T_times, T_div, T_mod








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

optional_T_times : /*nothing*/{ () }
                 | optional_T_times T_times { () }
;

basic_type : T_int  { () }
           | T_char { () }
           | T_bool { () }
           | T_double { () }
;

function_declaration : result_type T_id T_lparen optional_parameter_list T_rparen { () }
;

result_type : ttype { () }
            | T_void { () }
;

optional_parameter_list :  /*nothing*/ { () }
                        | parameter_list { () }
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
                | expression_list T_comma expression { () }
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
           | T_lparen after_lparen { () } 
           | T_true { () }
           | T_false { () }
           | T_NULL { () }
           | T_intconst { () }
           | T_charconst { () }
           | T_doubleconst { () }
           | T_stringliteral { () }
           | T_id T_lparen optional_expression_list T_rparen { () }
           | expression T_lbracket expression T_rbracket { () } 
           | unary_expression { () }
           | binary_expression { () }
           | unary_assignment { () }
           | binary_assignment { () }
           | T_lparen ttype T_rparen expression { () }
           | expression T_q expression T_colon { () }
           | T_new ttype optional_new { () }
           | T_delete expression { () }
;


after_lparen :  expression T_rparen { () }
             | ttype T_rparen { () }
;

unary_expression : T_bitand expression{ () }
           | T_times expression { () }
           | T_plus expression{ () }
           | T_minus expression { () }
           | T_bitnot expression { () }
;

binary_expression : expression T_times expression { () }
                  | expression T_div expression { () }
                  | expression T_mod expression { () }
                  | expression T_plus expression { () }
                  | expression T_minus expression { () }
                  | expression T_lt expression { () }
                  | expression T_gt expression { () }
                  | expression T_le expression { () }
                  | expression T_ge expression { () }
                  | expression T_eq expression { () }
                  | expression T_neq expression { () }
                  | expression T_and expression { () }
                  | expression T_or expression { () }
                  | expression T_comma expression{ () }
;

unary_assignment : T_plusplus expression { () }
                 | T_minusminus expression { () }
                 | expression T_plusplus { () }
                 | expression T_minusminus { () }
;

binary_assignment : expression T_assign expression{ () }
                  | expression T_timeseq expression{ () }
                  | expression T_diveq expression{ () }
                  | expression T_modeq expression{ () }
                  | expression T_pluseq expression{ () }
                  | expression T_minuseq expression{ () }

optional_new : /*nothing*/ { () }
             | T_lbracket expression T_rbracket { () }
;

constant_expression : expression { () } 
;